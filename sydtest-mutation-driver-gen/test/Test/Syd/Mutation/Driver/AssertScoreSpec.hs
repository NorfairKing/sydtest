{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Driver.AssertScoreSpec (spec) where

import qualified Data.Text as T
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
  ( MutationRunReport (..),
  )
import Test.Syd.Mutation.Driver.AssertScore
  ( AssertScoreResult (..),
    assertScoreResult,
  )
import Text.Colour (TerminalCapabilities (..), renderChunksText)

spec :: Spec
spec = describe "assertScoreResult" $ do
  it "passes when nothing survived and nothing is uncovered" $ do
    let report =
          MutationRunReport
            { mutationRunReportKilled = 5,
              mutationRunReportSurvived = 0,
              mutationRunReportTimedOut = 0,
              mutationRunReportUncovered = 0,
              mutationRunReportSkipped = 0,
              mutationRunReportGroups = []
            }
        result = assertScoreResult True report
    assertScoreFailed result `shouldBe` False
    T.unpack (renderChunksText WithoutColours (assertScoreHeader result))
      `shouldBe` "PASS: All 5 mutation(s) accounted for."

  it "fails when one mutation survived" $ do
    let report =
          MutationRunReport
            { mutationRunReportKilled = 4,
              mutationRunReportSurvived = 1,
              mutationRunReportTimedOut = 0,
              mutationRunReportUncovered = 0,
              mutationRunReportSkipped = 0,
              mutationRunReportGroups = []
            }
        result = assertScoreResult True report
    assertScoreFailed result `shouldBe` True
    T.unpack (renderChunksText WithoutColours (assertScoreHeader result))
      `shouldBe` "FAIL: 1 surviving, 0 uncovered out of 5 mutation(s)."

  it "fails when one mutation is uncovered and assertion is enabled" $ do
    let report =
          MutationRunReport
            { mutationRunReportKilled = 4,
              mutationRunReportSurvived = 0,
              mutationRunReportTimedOut = 0,
              mutationRunReportUncovered = 1,
              mutationRunReportSkipped = 0,
              mutationRunReportGroups = []
            }
    assertScoreFailed (assertScoreResult True report) `shouldBe` True

  it "passes when one mutation is uncovered but the uncovered assertion is disabled" $ do
    let report =
          MutationRunReport
            { mutationRunReportKilled = 4,
              mutationRunReportSurvived = 0,
              mutationRunReportTimedOut = 0,
              mutationRunReportUncovered = 1,
              mutationRunReportSkipped = 0,
              mutationRunReportGroups = []
            }
    assertScoreFailed (assertScoreResult False report) `shouldBe` False

  it "still fails on survivors even when the uncovered assertion is disabled" $ do
    let report =
          MutationRunReport
            { mutationRunReportKilled = 4,
              mutationRunReportSurvived = 1,
              mutationRunReportTimedOut = 0,
              mutationRunReportUncovered = 0,
              mutationRunReportSkipped = 0,
              mutationRunReportGroups = []
            }
    assertScoreFailed (assertScoreResult False report) `shouldBe` True

  it "counts killed + survived + uncovered (timed-out and skipped don't add to the total in the header)" $ do
    let report =
          MutationRunReport
            { mutationRunReportKilled = 3,
              mutationRunReportSurvived = 2,
              mutationRunReportTimedOut = 0,
              mutationRunReportUncovered = 1,
              mutationRunReportSkipped = 99,
              mutationRunReportGroups = []
            }
        result = assertScoreResult True report
    T.unpack (renderChunksText WithoutColours (assertScoreHeader result))
      `shouldBe` "FAIL: 2 surviving, 1 uncovered out of 6 mutation(s)."
