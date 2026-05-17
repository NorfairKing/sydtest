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
    map stepLabel steps `shouldBe` ["init"]
    map stepDeployTarget steps `shouldBe` ["init"]
    map stepScriptName steps `shouldBe` ["init"]
    map stepIsGrandfathered steps `shouldBe` [False]

  it "splits a reworked change into two steps" $ do
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
    map stepLabel steps `shouldBe` ["init", "add-foo@v1", "add-foo"]
    map stepDeployTarget steps `shouldBe` ["init", "@v1", "add-foo@HEAD"]
    map stepScriptName steps `shouldBe` ["init", "add-foo@v1", "add-foo"]

  it "marks every step as not grandfathered when no tag is given" $ do
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
    map stepIsGrandfathered steps `shouldBe` [False, False]

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
    map stepLabel steps `shouldBe` ["a", "b", "c", "d"]
    map stepIsGrandfathered steps `shouldBe` [True, True, False, False]

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
    map stepLabel steps `shouldBe` ["init", "add-color@v1", "add-color"]
