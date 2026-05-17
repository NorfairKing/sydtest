{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Sqitch.PlanSpec (spec) where

import Control.Exception (SomeException, try)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Path (parseRelFile, toFilePath, (</>))
import Path.IO (resolveDir', resolveFile')
import Test.Syd
import Test.Syd.Sqitch.Plan
import qualified UnliftIO.Temporary as Tmp

-- | Helper: write a plan to a temp file and parse it.
parsePlan :: Maybe Text -> Text -> IO [PlanStep]
parsePlan mTag body =
  Tmp.withSystemTempDirectory "sydtest-sqitch-plan" $ \dirStr -> do
    dir <- resolveDir' dirStr
    name <- parseRelFile "sqitch.plan"
    let path = dir </> name
    Text.writeFile (toFilePath path) body
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
