{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.Mutation.Driver.AssertScoreSpec (spec) where

import qualified Control.Exception as Exception
import qualified Data.Text as T
import Path
import Path.IO (doesFileExist, withSystemTempDir)
import System.Exit (ExitCode (..))
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
  ( MutationRunReport (..),
    writeMutationRunReport,
  )
import Test.Syd.Mutation.Driver.AssertScore
  ( AssertScoreResult (..),
    assertScoreResult,
    runAssertScore,
  )
import Text.Colour (TerminalCapabilities (..), renderChunksText)

spec :: Spec
spec = do
  describe "runAssertScore" $ do
    it "symlinks report.{txt,json} into the out dir on success" $
      withSystemTempDir "assert-score-pass" $ \dir -> do
        let reportDir = dir </> [reldir|report|]
            outDir = dir </> [reldir|out|]
            report =
              MutationRunReport
                { mutationRunReportKilled = 3,
                  mutationRunReportSurvived = 0,
                  mutationRunReportTimedOut = 0,
                  mutationRunReportUncovered = 0,
                  mutationRunReportSkipped = 0,
                  mutationRunReportGroups = []
                }
        writeMutationRunReport reportDir report
        -- The Nix harness writes report.txt alongside report.json; we
        -- emulate that here so the symlink target exists.
        writeFile (fromAbsFile (reportDir </> [relfile|report.txt|])) "(rendered report)"
        runAssertScore True reportDir (Just outDir)
        txtCopied <- doesFileExist (outDir </> [relfile|report.txt|])
        jsonCopied <- doesFileExist (outDir </> [relfile|report.json|])
        txtCopied `shouldBe` True
        jsonCopied `shouldBe` True

    it "exits 1 on a failed assertion and does not symlink anything into the out dir" $
      withSystemTempDir "assert-score-fail" $ \dir -> do
        let reportDir = dir </> [reldir|report|]
            outDir = dir </> [reldir|out|]
            report =
              MutationRunReport
                { mutationRunReportKilled = 3,
                  mutationRunReportSurvived = 1,
                  mutationRunReportTimedOut = 0,
                  mutationRunReportUncovered = 0,
                  mutationRunReportSkipped = 0,
                  mutationRunReportGroups = []
                }
        writeMutationRunReport reportDir report
        writeFile (fromAbsFile (reportDir </> [relfile|report.txt|])) "(rendered report)"
        result <-
          ( Exception.try ::
              IO () ->
              IO (Either ExitCode ())
          )
            $ runAssertScore True reportDir (Just outDir)
        result `shouldBe` Left (ExitFailure 1)
        -- The out dir should not exist (or should be empty) because
        -- the symlink step never ran.
        txtCopied <- doesFileExist (outDir </> [relfile|report.txt|])
        txtCopied `shouldBe` False

    it "exits 2 when report.json is missing" $
      withSystemTempDir "assert-score-missing" $ \dir -> do
        let reportDir = dir </> [reldir|empty|]
        result <-
          ( Exception.try ::
              IO () ->
              IO (Either ExitCode ())
          )
            $ runAssertScore True reportDir Nothing
        result `shouldBe` Left (ExitFailure 2)

  describe "assertScoreResult" $ do
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
