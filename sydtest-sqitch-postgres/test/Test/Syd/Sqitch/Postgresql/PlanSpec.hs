{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Sqitch.Postgresql.PlanSpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as SB
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Path
import Path.IO
import Test.Syd
import Test.Syd.Sqitch.Postgresql.Plan
import qualified UnliftIO.Temporary as Tmp

-- | Helper: write a plan to a temp file and parse it.
parsePlan :: Maybe Text -> Text -> IO [PlanStep]
parsePlan mTag body =
  Tmp.withSystemTempDirectory "sydtest-sqitch-plan" $ \dirStr -> do
    dir <- resolveDir' dirStr
    name <- parseRelFile "sqitch.plan"
    let path = dir </> name
    SB.writeFile (fromAbsFile path) (TE.encodeUtf8 body)
    readSqitchPlan mTag path

spec :: Spec
spec = describe "readSqitchPlan" $ do
  it "parses an empty plan to no steps" $ do
    steps <- parsePlan Nothing "%syntax-version=1.0.0\n%project=p\n\n"
    steps `shouldBe` []

  it "parses a single change" $ do
    steps <-
      parsePlan
        Nothing
        "%syntax-version=1.0.0\n%project=p\ninit 2026-01-01T00:00:00Z syd <syd@example.com> # x\n"
    steps
      `shouldBe` [ PlanStep
                     { stepLabel = "init",
                       stepDeployTarget = "init",
                       stepScriptName = "init",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     }
                 ]

  it "splits a reworked change into two steps and marks the head" $ do
    steps <-
      parsePlan
        Nothing
        ( Text.unlines
            [ "%syntax-version=1.0.0",
              "%project=p",
              "init 2026-01-01T00:00:00Z syd <syd@example.com> # x",
              "add-foo 2026-01-02T00:00:00Z syd <syd@example.com> # y",
              "@v1 2026-01-03T00:00:00Z syd <syd@example.com> # tag",
              "add-foo [add-foo@v1] 2026-01-04T00:00:00Z syd <syd@example.com> # rework"
            ]
        )
    steps
      `shouldBe` [ PlanStep
                     { stepLabel = "init",
                       stepDeployTarget = "init",
                       stepScriptName = "init",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "add-foo@v1",
                       stepDeployTarget = "@v1",
                       stepScriptName = "add-foo@v1",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "add-foo",
                       stepDeployTarget = "add-foo@HEAD",
                       stepScriptName = "add-foo",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = True
                     }
                 ]

  it "marks no step as grandfathered when no tag is given" $ do
    steps <-
      parsePlan
        Nothing
        ( Text.unlines
            [ "%project=p",
              "a 2026-01-01T00:00:00Z syd <syd@example.com> # x",
              "@v 2026-01-02T00:00:00Z syd <syd@example.com> # tag",
              "b 2026-01-03T00:00:00Z syd <syd@example.com> # x"
            ]
        )
    steps
      `shouldBe` [ PlanStep
                     { stepLabel = "a",
                       stepDeployTarget = "a",
                       stepScriptName = "a",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "b",
                       stepDeployTarget = "b",
                       stepScriptName = "b",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     }
                 ]

  it "marks steps at or before the tag as grandfathered, others not" $ do
    steps <-
      parsePlan
        (Just "legacy")
        ( Text.unlines
            [ "%project=p",
              "a 2026-01-01T00:00:00Z syd <syd@example.com> # x",
              "b 2026-01-02T00:00:00Z syd <syd@example.com> # x",
              "@legacy 2026-01-03T00:00:00Z syd <syd@example.com> # tag",
              "c 2026-01-04T00:00:00Z syd <syd@example.com> # x",
              "d 2026-01-05T00:00:00Z syd <syd@example.com> # x"
            ]
        )
    steps
      `shouldBe` [ PlanStep
                     { stepLabel = "a",
                       stepDeployTarget = "a",
                       stepScriptName = "a",
                       stepIsGrandfathered = True,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "b",
                       stepDeployTarget = "b",
                       stepScriptName = "b",
                       stepIsGrandfathered = True,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "c",
                       stepDeployTarget = "c",
                       stepScriptName = "c",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "d",
                       stepDeployTarget = "d",
                       stepScriptName = "d",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     }
                 ]

  it "marks every step as grandfathered when the tag is the last line" $ do
    steps <-
      parsePlan
        (Just "legacy")
        ( Text.unlines
            [ "%project=p",
              "a 2026-01-01T00:00:00Z syd <syd@example.com> # x",
              "b 2026-01-02T00:00:00Z syd <syd@example.com> # x",
              "@legacy 2026-01-03T00:00:00Z syd <syd@example.com> # tag"
            ]
        )
    map stepIsGrandfathered steps `shouldBe` [True, True]

  it "marks no step as grandfathered when the tag is the very first thing in the plan" $ do
    steps <-
      parsePlan
        (Just "legacy")
        ( Text.unlines
            [ "%project=p",
              "@legacy 2026-01-01T00:00:00Z syd <syd@example.com> # tag",
              "a 2026-01-02T00:00:00Z syd <syd@example.com> # x",
              "b 2026-01-03T00:00:00Z syd <syd@example.com> # x"
            ]
        )
    map stepIsGrandfathered steps `shouldBe` [False, False]

  it "grandfathers the rework predecessor but not the head when the tag IS the rework tag" $ do
    -- The classic scenario: a change was reworked, and the tag the
    -- rework predates is also the cut-over for idempotence. The
    -- predecessor (deployed pre-tag) is grandfathered; the rework
    -- head (deployed post-tag) is not.
    steps <-
      parsePlan
        (Just "v1")
        ( Text.unlines
            [ "%project=p",
              "init 2026-01-01T00:00:00Z syd <syd@example.com> # x",
              "@v1 2026-01-02T00:00:00Z syd <syd@example.com> # tag",
              "init [init@v1] 2026-01-03T00:00:00Z syd <syd@example.com> # rework"
            ]
        )
    steps
      `shouldBe` [ PlanStep
                     { stepLabel = "init@v1",
                       stepDeployTarget = "@v1",
                       stepScriptName = "init@v1",
                       stepIsGrandfathered = True,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "init",
                       stepDeployTarget = "init@HEAD",
                       stepScriptName = "init",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = True
                     }
                 ]

  it "handles a change reworked twice (three occurrences, two tags)" $ do
    -- The first two occurrences are tagged predecessors; only the
    -- third (and last) is the rework head with target @HEAD.
    steps <-
      parsePlan
        Nothing
        ( Text.unlines
            [ "%project=p",
              "foo 2026-01-01T00:00:00Z syd <syd@example.com> # original",
              "@v1 2026-01-02T00:00:00Z syd <syd@example.com> # tag1",
              "foo [foo@v1] 2026-01-03T00:00:00Z syd <syd@example.com> # rework 1",
              "@v2 2026-01-04T00:00:00Z syd <syd@example.com> # tag2",
              "foo [foo@v2] 2026-01-05T00:00:00Z syd <syd@example.com> # rework 2"
            ]
        )
    steps
      `shouldBe` [ PlanStep
                     { stepLabel = "foo@v1",
                       stepDeployTarget = "@v1",
                       stepScriptName = "foo@v1",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "foo@v2",
                       stepDeployTarget = "@v2",
                       stepScriptName = "foo@v2",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "foo",
                       stepDeployTarget = "foo@HEAD",
                       stepScriptName = "foo",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = True
                     }
                 ]

  it "ignores blank lines and lines with only whitespace" $ do
    steps <-
      parsePlan
        Nothing
        ( Text.unlines
            [ "%project=p",
              "",
              "  ",
              "\t",
              "a 2026-01-01T00:00:00Z syd <syd@example.com> # x",
              "",
              "b 2026-01-02T00:00:00Z syd <syd@example.com> # x"
            ]
        )
    map stepLabel steps `shouldBe` ["a", "b"]

  it "fails when the grandfather tag is not present in the plan" $ do
    res <-
      try $
        parsePlan
          (Just "nope")
          "%project=p\ninit 2026-01-01T00:00:00Z syd <syd@example.com> # x\n"
    case res of
      Left (e :: SomeException) ->
        ("nope" `isInfixOf` show e) `shouldBe` True
      Right _ -> expectationFailure "expected readSqitchPlan to fail"

  it "parses the toy-sqitch-ok fixture file from disk" $ do
    p <- resolveFile' "test_resources/toy-sqitch-ok/sqitch.plan"
    steps <- readSqitchPlan Nothing p
    steps
      `shouldBe` [ PlanStep
                     { stepLabel = "init",
                       stepDeployTarget = "init",
                       stepScriptName = "init",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "add-color@v1",
                       stepDeployTarget = "@v1",
                       stepScriptName = "add-color@v1",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = False
                     },
                   PlanStep
                     { stepLabel = "add-color",
                       stepDeployTarget = "add-color@HEAD",
                       stepScriptName = "add-color",
                       stepIsGrandfathered = False,
                       stepIsReworkHead = True
                     }
                 ]
