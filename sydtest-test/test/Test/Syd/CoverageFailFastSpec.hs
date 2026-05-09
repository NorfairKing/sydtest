{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the fail-fast path in 'runSingleCoverageMode': when fail-fast
-- is on, a test failure during the coverage phase (where no mutation is
-- active yet) must abort the run rather than silently producing coverage
-- data against a broken baseline.
--
-- The coverage child signals "tests failed during coverage" to the parent
-- by exiting with code 2; the parent treats that as a non-retryable abort
-- and exits non-zero.  These tests exercise the child path directly by
-- catching the 'ExitCode' that 'exitWith' throws.
module Test.Syd.CoverageFailFastSpec (spec) where

import Control.Exception (try)
import Path
import Path.IO (withSystemTempDir)
import System.Exit (ExitCode (..))
import Test.Syd
import Test.Syd.MutationMode (runSingleCoverageMode)
import Test.Syd.OptParse
  ( CoverageChildSettings (..),
    defaultSettings,
  )

spec :: Spec
spec = describe "runSingleCoverageMode fail-fast" $ do
  it "exits with code 2 when a covered test fails and fail-fast is on" $
    withSystemTempDir "coverage-fail-fast" $ \tmpDir -> do
      let outputFile = fromAbsFile (tmpDir </> [relfile|coverage.json|])
          baselineFile = fromAbsFile (tmpDir </> [relfile|baseline.json|])
          covChild =
            CoverageChildSettings
              { coverageChildTestId = "always-fails",
                coverageChildOutput = outputFile,
                coverageChildBaselineOutput = baselineFile,
                coverageChildSuiteName = Nothing
              }
          failingSpec :: Spec
          failingSpec = it "always-fails" (expectationFailure "intentional" :: IO ())
      result <- try (runSingleCoverageMode defaultSettings True covChild failingSpec)
      case result of
        Left (ExitFailure 2) -> pure ()
        Left ec -> expectationFailure ("expected ExitFailure 2, got " <> show ec)
        Right () -> expectationFailure "expected exitWith, but the call returned"

  it "returns normally when a covered test passes and fail-fast is on" $
    withSystemTempDir "coverage-fail-fast" $ \tmpDir -> do
      let outputFile = fromAbsFile (tmpDir </> [relfile|coverage.json|])
          baselineFile = fromAbsFile (tmpDir </> [relfile|baseline.json|])
          covChild =
            CoverageChildSettings
              { coverageChildTestId = "always-passes",
                coverageChildOutput = outputFile,
                coverageChildBaselineOutput = baselineFile,
                coverageChildSuiteName = Nothing
              }
          passingSpec :: Spec
          passingSpec = it "always-passes" (pure () :: IO ())
      result <- try (runSingleCoverageMode defaultSettings True covChild passingSpec)
      case result of
        Left (ec :: ExitCode) ->
          expectationFailure ("expected normal return, got " <> show ec)
        Right () -> pure ()

  it "returns normally when a covered test fails but fail-fast is off" $
    withSystemTempDir "coverage-fail-fast" $ \tmpDir -> do
      let outputFile = fromAbsFile (tmpDir </> [relfile|coverage.json|])
          baselineFile = fromAbsFile (tmpDir </> [relfile|baseline.json|])
          covChild =
            CoverageChildSettings
              { coverageChildTestId = "always-fails",
                coverageChildOutput = outputFile,
                coverageChildBaselineOutput = baselineFile,
                coverageChildSuiteName = Nothing
              }
          failingSpec :: Spec
          failingSpec = it "always-fails" (expectationFailure "intentional" :: IO ())
      result <- try (runSingleCoverageMode defaultSettings False covChild failingSpec)
      case result of
        Left (ec :: ExitCode) ->
          expectationFailure ("expected normal return, got " <> show ec)
        Right () -> pure ()
