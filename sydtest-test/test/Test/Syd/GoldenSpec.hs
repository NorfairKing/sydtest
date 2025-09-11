{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.GoldenSpec (spec) where

import Control.Monad.Writer
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Test.Syd
import Test.Syd.OptParse
import Text.Colour

spec :: Spec
spec = do
  describe "outputResultForest" $ do
    it "outputs the same as last time" $ do
      pureGoldenTextFile
        "test_resources/output.golden"
        ( LT.toStrict $
            LTB.toLazyText $
              renderResultReport
                defaultSettings
                With24BitColours
                ( Timed
                    { timedValue = [],
                      timedWorker = 0,
                      timedBegin = 0,
                      timedEnd = 0
                    }
                )
        )
  describe "defaultSettings" $ do
    it "is the same thing as last time" $ goldenPrettyShowInstance "test_resources/defaultSettings-show.golden" defaultSettings

  describe "StagedGolden" $ do
    it "passes when there are no stages" $ stagedGolden $ \_ -> do
      pure ()

    it "can run multiple staged golden phases in a custom monad" $ stagedGolden $ \goldenStage -> do
      ((), logs) <- runWriterT $ do
        tell ["log 1"]
        goldenStage $ pureGoldenTextFile "test_resources/staged-golden/1.golden" "output 1"
        tell ["log 2"]
        goldenStage $ pureGoldenTextFile "test_resources/staged-golden/2.golden" "output 2"
      length (logs :: [String]) `shouldBe` 2

    it "does not crash the framework" $ do
      let failingSpec = do
            specify "missing file" $ stagedGolden $ \goldenStage ->
              goldenStage $ pureGoldenTextFile "test_resources/staged-golden/definitely-missing" "output"

            specify "wrong file" $ stagedGolden $ \goldenStage ->
              goldenStage $ pureGoldenTextFile "test_resources/staged-golden/1.golden" "output"

            specify "exception" $ stagedGolden $ \goldenStage ->
              goldenStage $ error "This is an exception instead of a golden test"

            specify "exception" $ stagedGolden $ \goldenStage -> do
              goldenStage $ pureGoldenTextFile (error "This is an exception instead of a file name") "output"

            specify "exception" $ stagedGolden $ \goldenStage -> do
              goldenStage $ pureGoldenTextFile "test_resources/staged-golden/nonexistent" (error "This is an exception instead of a text")

            -- The test should fail on the first failing golden test and then
            -- stop, not continue until all golden tests have been output.
            specify "infinite-failing" $ stagedGolden $ \goldenStage -> do
              let go = do
                    -- Failing golden test.
                    goldenStage $ pureGoldenTextFile "test_resources/staged-golden/1.golden" "output"
                    go
              go

      _ <- sydTestResult defaultSettings failingSpec
      pure ()
