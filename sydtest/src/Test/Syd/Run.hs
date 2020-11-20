{-# LANGUAGE DeriveGeneric #-}
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
  runTest :: TestRunSettings -> (forall r. (Arg e -> IO r) -> IO r) -> e -> ResourceT IO TestRunResult

instance IsTest Bool where
  type Arg Bool = ()
  runTest = runPureTest

runPureTest :: TestRunSettings -> (forall r. (() -> IO r) -> IO r) -> Bool -> ResourceT IO TestRunResult
runPureTest TestRunSettings {..} wrapper b = do
  let testRunResultNumTests = Nothing
  let runInChildProcess = fromMaybe False testRunSettingChildProcessOverride
  let runWrapper = if runInChildProcess then runInSilencedProcess else id
  (testRunResultExecutionTime, resultBool) <- timeItT
    $ runWrapper
    $ liftIO
    $ wrapper
    $ \() -> (Right <$> evaluate b) `catches` exceptionHandlers
  let (testRunResultStatus, testRunResultException) = case resultBool of
        Left ex -> (TestFailed, Just ex)
        Right bool -> (if bool then TestPassed else TestFailed, Nothing)
  let testRunResultNumShrinks = Nothing
  pure TestRunResult {..}

instance IsTest (IO a) where
  type Arg (IO a) = ()
  runTest = runIOTest

runIOTest :: TestRunSettings -> (forall r. (() -> IO r) -> IO r) -> IO a -> ResourceT IO TestRunResult
runIOTest TestRunSettings {..} wrapper func = do
  let testRunResultNumTests = Nothing
  let runInChildProcess = fromMaybe False testRunSettingChildProcessOverride
  let runWrapper = if runInChildProcess then runInSilencedProcess else id
  (testRunResultExecutionTime, (testRunResultStatus, testRunResultException)) <-
    timeItT
      $ runWrapper
      $ liftIO
      $ wrapper
      $ \() -> do
        result <-
          (liftIO func >>= (evaluate . (`seq` Right TestPassed)))
            `catches` exceptionHandlers
        pure $ case result of
          Left ex -> (TestFailed, Just ex)
          Right r -> (r, Nothing)
  let testRunResultNumShrinks = Nothing
  pure TestRunResult {..}

instance IsTest Property where
  type Arg Property = ()
  runTest = runPropertyTest

runPropertyTest :: TestRunSettings -> (forall r. (() -> IO r) -> IO r) -> Property -> ResourceT IO TestRunResult
runPropertyTest TestRunSettings {..} wrapper p = do
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
  runWrapper $ liftIO $ wrapper $ \() -> do
    (testRunResultExecutionTime, result) <- timeItT $ liftIO $ quickCheckWithResult args p
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

data Assertion = Equality String String
  deriving (Show, Eq, Typeable, Generic)

instance Exception Assertion
