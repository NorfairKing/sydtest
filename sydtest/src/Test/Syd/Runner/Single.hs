{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Runner.Single (runSingleTestWithFlakinessMode, reportUnrenderableFailures) where

import Control.DeepSeq (force)
import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Text (Text)
import System.Timeout (timeout)
import Test.Syd.HList
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.SpecDef
import Text.Colour (chunkText)

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
  -- | Timeout
  Timeout ->
  -- | Max retries
  Word ->
  -- | Flakiness mode
  FlakinessMode ->
  -- | Expectation mode
  ExpectationMode ->
  -- | Test result
  IO TestRunReport
runSingleTestWithFlakinessMode progressReporter l td mTimeout maxRetries fm em = do
  rawResults <- runSingleTestWithRetries progressReporter l td mTimeout maxRetries em
  results <- traverse reportUnrenderableFailures rawResults
  pure
    TestRunReport
      { testRunReportExpectationMode = em,
        testRunReportRawResults = results,
        testRunReportFlakinessMode = fm
      }

-- | Render what a result says about its test here, while it is still this
-- test's result.
--
-- Rendering a failure runs user code: the 'displayException' of whatever the
-- test threw, the 'Show' of a compared value, a message built lazily.  Left to
-- the thread that collects results, the first rendering happens there, and
-- anything that throws takes the suite down after every test has already run,
-- along with the summary and all the other results.  Forced here, it is a
-- failure of the test that produced it, which is whose fault it is.
--
-- Rendering is pure, so forcing it once is enough: the output will be
-- rendering the same value again, and a value that rendered once renders
-- again.
reportUnrenderableFailures :: TestRunResult -> IO TestRunResult
reportUnrenderableFailures result = do
  errOrRendered <- try $ evaluate $ force $ renderingOf result
  case errOrRendered of
    Right _ -> pure result
    Left e -> do
      why <- describeUnrenderable e
      -- Nothing of what could not be rendered is kept: a result that still
      -- carries it carries something that throws when the output reaches it,
      -- which is the whole problem.  What replaces it is an assertion holding
      -- the message, because that is the failure both output formats render as
      -- the text it is, with no exception context to drag a build-dependent
      -- backtrace along.
      pure
        result
          { testRunResultException = Just (toException (ExpectationFailed why)),
            testRunResultExtraInfo = Nothing,
            testRunResultFailingInputs = [],
            testRunResultLabels = Nothing,
            testRunResultClasses = Nothing,
            testRunResultTables = Nothing
          }
  where
    renderingOf :: TestRunResult -> ([Text], Maybe String, [String], Maybe (Map [String] Int), Maybe (Map String Int), Maybe (Map String (Map String Int)))
    renderingOf TestRunResult {..} =
      ( map chunkText (concat (maybe [] outputSomeException testRunResultException)),
        testRunResultExtraInfo,
        testRunResultFailingInputs,
        testRunResultLabels,
        testRunResultClasses,
        testRunResultTables
      )

    -- Unwrapped, because 'displayException' of a 'SomeException' renders the
    -- exception's context as well, and a backtrace through the rendering says
    -- nothing that the exception itself does not.
    describeUnrenderable :: SomeException -> IO String
    describeUnrenderable (SomeException e) = do
      errOrWhy <- try $ evaluate $ force $ displayException e
      pure $
        unwords
          [ "This test failed, but its failure could not be rendered:",
            case errOrWhy of
              Right why -> why
              Left (_ :: SomeException) -> "and neither could the exception from rendering it."
          ]

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
  -- | Timeout
  Timeout ->
  -- | Max retries
  Word ->
  -- | Expectation mode
  ExpectationMode ->
  -- If the test ever passed, and the last test result
  IO (NonEmpty TestRunResult)
runSingleTestWithRetries progressReporter l td mTimeout maxRetries em = go maxRetries
  where
    go :: Word -> IO (NonEmpty TestRunResult)
    go w
      | w <= 1 = (:| []) . either id id <$> runWithTimeout
      | otherwise = do
          mResult <- runWithTimeout
          case mResult of
            -- Don't retry on timeout
            Left result -> pure (result :| [])
            Right result ->
              if testStatusMatchesExpectationMode (testRunResultStatus result) em
                then pure (result :| [])
                else do
                  rest <- go (pred w)
                  pure (result NE.<| rest)
      where
        runWithTimeout :: IO (Either TestRunResult TestRunResult)
        runWithTimeout = case mTimeout of
          DoNotTimeout -> Right <$> runFunc
          TimeoutAfterMicros micros -> do
            mResult <- timeout micros runFunc
            pure $ case mResult of
              Nothing -> Left timeoutResult
              Just result -> Right result

        runFunc :: IO TestRunResult
        runFunc = testDefVal td progressReporter (\f -> f l ())

        timeoutResult :: TestRunResult
        timeoutResult =
          TestRunResult
            { testRunResultStatus = TestFailed,
              testRunResultException = Nothing,
              testRunResultNumTests = Nothing,
              testRunResultNumShrinks = Nothing,
              testRunResultFailingInputs = [],
              testRunResultLabels = Nothing,
              testRunResultClasses = Nothing,
              testRunResultTables = Nothing,
              testRunResultGoldenCase = Nothing,
              testRunResultExtraInfo = Just "Timeout!"
            }
