{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Runner.Single (runSingleTestWithFlakinessMode) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Test.Syd.HList
import Test.Syd.Run
import Test.Syd.SpecDef

-- | Run a single test.
--
-- Run the test up to 'maxRetries' times.
-- Finish as soon as the test passes once, or when we run out of retries.
runSingleTestWithFlakinessMode ::
  forall externalResources t.
  -- | How to report test progress
  ProgressReporter ->
  -- | External resources
  HList externalResources ->
  -- | Test definition
  TDef
    ( ProgressReporter ->
      ((HList externalResources -> () -> t) -> t) ->
      IO TestRunResult
    ) ->
  -- | Max retries
  Word ->
  -- | Flakiness mode
  FlakinessMode ->
  -- | Test result
  IO TestRunReport
runSingleTestWithFlakinessMode progressReporter l td maxRetries fm = do
  results <- runSingleTestWithRetries progressReporter l td maxRetries
  pure
    TestRunReport
      { testRunReportRawResults = results,
        testRunReportFlakinessMode = fm
      }

runSingleTestWithRetries ::
  forall externalResources t.
  -- | How to report test progress
  ProgressReporter ->
  -- | External resources
  HList externalResources ->
  -- | Test definition
  TDef
    ( ProgressReporter ->
      ((HList externalResources -> () -> t) -> t) ->
      IO TestRunResult
    ) ->
  -- | Max retries
  Word ->
  -- If the test ever passed, and the last test result
  IO (NonEmpty TestRunResult)
runSingleTestWithRetries progressReporter l td maxRetries = go maxRetries
  where
    go :: Word -> IO (NonEmpty TestRunResult)
    go w
      | w <= 1 = (:| []) <$> runFunc
      | otherwise = do
        result <- runFunc
        case testRunResultStatus result of
          TestPassed -> pure (result :| [])
          TestFailed -> do
            rest <- go (pred w)
            pure (result NE.<| rest)
      where
        runFunc :: IO TestRunResult
        runFunc = testDefVal td progressReporter (\f -> f l ())
