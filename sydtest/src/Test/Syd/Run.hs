{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Run where

import Control.Exception hiding (Handler, catches, evaluate)
import Data.Maybe
import Data.Typeable
import GHC.Generics (Generic)
import System.TimeIt
import Test.QuickCheck
import Test.QuickCheck.IO ()
import Test.QuickCheck.Random
import Test.Syd.Silence
import UnliftIO
import UnliftIO.Resource

class IsTest e where
  type Arg e
  type Arg e = ()
  runTest :: TestRunSettings -> ((Arg e -> IO ()) -> IO ()) -> e -> ResourceT IO TestRunResult

instance IsTest Bool where
  type Arg Bool = ()
  runTest = runPureTest

runPureTest :: TestRunSettings -> ((() -> IO ()) -> IO ()) -> Bool -> ResourceT IO TestRunResult
runPureTest sets wrapper bool = runPureTestWithArg sets wrapper (\() -> bool)

instance IsTest (arg -> Bool) where
  type Arg (arg -> Bool) = arg
  runTest = runPureTestWithArg

runPureTestWithArg :: TestRunSettings -> ((arg -> IO ()) -> IO ()) -> (arg -> Bool) -> ResourceT IO TestRunResult
runPureTestWithArg TestRunSettings {..} wrapper computeBool = do
  let testRunResultNumTests = Nothing
  let runInChildProcess = fromMaybe False testRunSettingChildProcessOverride
  let runWrapper = if runInChildProcess then runInSilencedProcess else id
  (testRunResultExecutionTime, resultBool) <- timeItT
    $ runWrapper
    $ liftIO
    $ applyWrapper wrapper
    $ \arg -> (Right <$> evaluate (computeBool arg)) `catches` exceptionHandlers
  let (testRunResultStatus, testRunResultException) = case resultBool of
        Left ex -> (TestFailed, Just ex)
        Right bool -> (if bool then TestPassed else TestFailed, Nothing)
  let testRunResultNumShrinks = Nothing
  pure TestRunResult {..}

applyWrapper :: ((arg -> IO ()) -> IO ()) -> (arg -> IO r) -> IO r
applyWrapper wrapper func = do
  var <- newEmptyMVar
  wrapper $ \arg -> do
    res <- func arg
    putMVar var res
  readMVar var

instance IsTest (IO a) where
  type Arg (IO a) = ()
  runTest = runIOTest

runIOTest :: TestRunSettings -> ((() -> IO ()) -> IO ()) -> IO a -> ResourceT IO TestRunResult
runIOTest sets wrapper func = runIOTestWithArg sets wrapper (\() -> func)

instance IsTest (arg -> IO a) where
  type Arg (arg -> IO a) = arg
  runTest = runIOTestWithArg

runIOTestWithArg :: TestRunSettings -> ((arg -> IO ()) -> IO ()) -> (arg -> IO a) -> ResourceT IO TestRunResult
runIOTestWithArg TestRunSettings {..} wrapper func = do
  let testRunResultNumTests = Nothing
  let runInChildProcess = fromMaybe False testRunSettingChildProcessOverride
  let runWrapper = if runInChildProcess then runInSilencedProcess else id
  (testRunResultExecutionTime, (testRunResultStatus, testRunResultException)) <-
    timeItT
      $ runWrapper
      $ liftIO
      $ applyWrapper wrapper
      $ \arg -> do
        result <-
          (liftIO (func arg) >>= (evaluate . (`seq` Right TestPassed)))
            `catches` exceptionHandlers
        pure $ case result of
          Left ex -> (TestFailed, Just ex)
          Right r -> (r, Nothing)
  let testRunResultNumShrinks = Nothing
  pure TestRunResult {..}

instance IsTest Property where
  type Arg Property = ()
  runTest = runPropertyTest

runPropertyTest :: TestRunSettings -> ((() -> IO ()) -> IO ()) -> Property -> ResourceT IO TestRunResult
runPropertyTest sets wrapper p = runPropertyTestWithArg sets wrapper (\() -> p)

instance IsTest (arg -> Property) where
  type Arg (arg -> Property) = arg
  runTest = runPropertyTestWithArg

runPropertyTestWithArg :: TestRunSettings -> ((arg -> IO ()) -> IO ()) -> (arg -> Property) -> ResourceT IO TestRunResult
runPropertyTestWithArg TestRunSettings {..} wrapper p = do
  let args =
        stdArgs
          { replay = Just (mkQCGen testRunSettingSeed, 0),
            chatty = False,
            maxSuccess = testRunSettingMaxSuccess,
            maxDiscardRatio = testRunSettingMaxDiscardRatio,
            maxSize = testRunSettingMaxSize,
            maxShrinks = testRunSettingMaxShrinks
          }
  let runInChildProcess = fromMaybe False testRunSettingChildProcessOverride
  let runWrapper = if runInChildProcess then runInSilencedProcess else id
  runWrapper $ liftIO $ applyWrapper wrapper $ \arg -> do
    (testRunResultExecutionTime, result) <- timeItT $ liftIO $ quickCheckWithResult args (p arg)
    testRunResultStatus <- pure $ case result of
      Success {} -> TestPassed
      GaveUp {} -> TestFailed
      Failure {} -> TestFailed
      NoExpectedFailure {} -> TestFailed
    let testRunResultNumTests = Just $ fromIntegral $ numTests result
    let testRunResultNumShrinks = case result of
          Failure {} -> Just $ fromIntegral $ numShrinks result
          _ -> Nothing
    let testRunResultException = case result of
          Failure {} -> do
            se <- theException result
            pure $ case fromException se of
              Just a -> Right a
              Nothing -> Left $ displayException se
          _ -> Nothing
    pure TestRunResult {..}

exceptionHandlers :: MonadUnliftIO m => [Handler m (Either (Either String Assertion) a)]
exceptionHandlers =
  [ -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT (ctrl-c).
    Handler (\e -> throw (e :: AsyncException)),
    -- Catch assertions first because we know what to do with them.
    Handler $ \(a :: Assertion) -> pure (Left $ Right a),
    -- Catch all the rest as a string
    Handler (\e -> return $ Left (Left (displayException (e :: SomeException))))
  ]

type Test = IO ()

data TestRunSettings
  = TestRunSettings
      { testRunSettingChildProcessOverride :: Maybe Bool, -- Nothing means use the default, the specific test can decide what that is.
        testRunSettingSeed :: Int,
        testRunSettingMaxSuccess :: Int,
        testRunSettingMaxDiscardRatio :: Int,
        testRunSettingMaxSize :: Int,
        testRunSettingMaxShrinks :: Int
      }
  deriving (Show, Generic)

defaultSettings :: TestRunSettings
defaultSettings =
  TestRunSettings
    { testRunSettingChildProcessOverride = Nothing,
      testRunSettingSeed = 42,
      testRunSettingMaxSuccess = maxSuccess stdArgs,
      testRunSettingMaxDiscardRatio = maxDiscardRatio stdArgs,
      testRunSettingMaxSize = maxSize stdArgs,
      testRunSettingMaxShrinks = 100 -- This is different from what quickcheck does so that test suites are more likely to finish
    }

data TestRunResult
  = TestRunResult
      { testRunResultStatus :: !TestStatus,
        testRunResultException :: !(Maybe (Either String Assertion)),
        testRunResultNumTests :: !(Maybe Word),
        testRunResultNumShrinks :: !(Maybe Word),
        testRunResultExecutionTime :: !Double -- In seconds
      }
  deriving (Show, Generic)

data TestStatus = TestPassed | TestFailed
  deriving (Show, Eq, Generic)

data Assertion
  = NotEqualButShouldHaveBeenEqual String String
  | EqualButShouldNotHaveBeenEqual String String
  | PredicateSucceededButShouldHaveFailed String
  | PredicateFailedButShouldHaveSucceeded String
  deriving (Show, Eq, Typeable, Generic)

instance Exception Assertion
