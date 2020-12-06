{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines the 'IsTest' class and the different instances for it.
module Test.Syd.Run where

import Control.DeepSeq
import Control.Exception hiding (Handler, catches, evaluate)
import Data.Maybe
import Data.Typeable
import GHC.Generics (Generic)
import System.TimeIt
import Test.QuickCheck
import Test.QuickCheck.IO ()
import Test.QuickCheck.Random
import Test.Syd.HList
import Test.Syd.Silence
import UnliftIO

class IsTest e where
  type Arg1 e
  type Arg2 e
  runTest ::
    e ->
    TestRunSettings ->
    ((Arg1 e -> Arg2 e -> IO ()) -> IO ()) ->
    IO TestRunResult

instance IsTest Bool where
  type Arg1 Bool = HList '[] -- The argument from 'aroundAll'
  type Arg2 Bool = () -- The argument from 'around'
  runTest func = runTest (\(HNil :: HList '[]) () -> func)

instance IsTest (arg -> Bool) where
  type Arg1 (arg -> Bool) = HList '[]
  type Arg2 (arg -> Bool) = arg
  runTest func = runTest (\(HNil :: HList '[]) arg -> func arg)

instance IsTest (arg1 -> arg2 -> Bool) where
  type Arg1 (arg1 -> arg2 -> Bool) = arg1
  type Arg2 (arg1 -> arg2 -> Bool) = arg2
  runTest = runPureTestWithArg

runPureTestWithArg ::
  (arg1 -> arg2 -> Bool) ->
  TestRunSettings ->
  ((arg1 -> arg2 -> IO ()) -> IO ()) ->
  IO TestRunResult
runPureTestWithArg computeBool TestRunSettings {..} wrapper = do
  let testRunResultNumTests = Nothing
  let runInChildProcess = fromMaybe False testRunSettingChildProcessOverride
  let runWrapper = if runInChildProcess then runInSilencedProcess else id
  (testRunResultExecutionTime, resultBool) <- timeItT $
    runWrapper $ do
      liftIO $
        applyWrapper2 wrapper $
          \arg1 arg2 -> (Right <$> evaluate (computeBool arg1 arg2)) `catches` exceptionHandlers
  let (testRunResultStatus, testRunResultException) = case resultBool of
        Left ex -> (TestFailed, Just ex)
        Right bool -> (if bool then TestPassed else TestFailed, Nothing)
  let testRunResultNumShrinks = Nothing
  pure TestRunResult {..}

applyWrapper2 ::
  MonadIO m =>
  ((arg1 -> arg2 -> m ()) -> m ()) ->
  (arg1 -> arg2 -> m r) ->
  m r
applyWrapper2 wrapper func = do
  var <- liftIO $ newEmptyMVar
  wrapper $ \arg1 arg2 -> do
    res <- func arg1 arg2
    liftIO $ putMVar var res
  liftIO $ readMVar var

instance IsTest (IO a) where
  type Arg1 (IO a) = HList '[]
  type Arg2 (IO a) = ()
  runTest func = runTest (\(HNil :: HList '[]) () -> func)

instance IsTest (arg -> IO a) where
  type Arg1 (arg -> IO a) = HList '[]
  type Arg2 (arg -> IO a) = arg
  runTest func = runTest (\(HNil :: HList '[]) -> func)

instance IsTest (arg1 -> arg2 -> IO a) where
  type Arg1 (arg1 -> arg2 -> IO a) = arg1
  type Arg2 (arg1 -> arg2 -> IO a) = arg2
  runTest = runIOTestWithArg

runIOTestWithArg ::
  (arg1 -> arg2 -> IO a) ->
  TestRunSettings ->
  ((arg1 -> arg2 -> IO ()) -> IO ()) ->
  IO TestRunResult
runIOTestWithArg func TestRunSettings {..} wrapper = do
  let testRunResultNumTests = Nothing
  let runInChildProcess = fromMaybe False testRunSettingChildProcessOverride
  let runWrapper = if runInChildProcess then runInSilencedProcess else id
  (testRunResultExecutionTime, (testRunResultStatus, testRunResultException)) <-
    timeItT $
      runWrapper $
        liftIO $
          applyWrapper2 wrapper $
            \arg1 arg2 -> do
              result <-
                (liftIO (func arg1 arg2) >>= (evaluate . force . (`seq` Right TestPassed)))
                  `catches` exceptionHandlers
              evaluate $ case result of
                Left ex -> (TestFailed, Just ex)
                Right r -> (r, Nothing)
  let testRunResultNumShrinks = Nothing
  pure TestRunResult {..}

instance IsTest Property where
  type Arg1 Property = HList '[]
  type Arg2 Property = ()
  runTest func = runTest (\(HNil :: HList '[]) () -> func)

instance IsTest (arg -> Property) where
  type Arg1 (arg -> Property) = HList '[]
  type Arg2 (arg -> Property) = arg
  runTest func = runTest (\(HNil :: HList '[]) -> func)

instance IsTest (arg1 -> arg2 -> Property) where
  type Arg1 (arg1 -> arg2 -> Property) = arg1
  type Arg2 (arg1 -> arg2 -> Property) = arg2
  runTest = runPropertyTestWithArg

runPropertyTestWithArg ::
  (arg1 -> arg2 -> Property) ->
  TestRunSettings ->
  ((arg1 -> arg2 -> IO ()) -> IO ()) ->
  IO TestRunResult
runPropertyTestWithArg p TestRunSettings {..} wrapper = do
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
  runWrapper $
    liftIO $
      applyWrapper2 wrapper $ \arg1 arg2 -> do
        (testRunResultExecutionTime, result) <- timeItT $ liftIO $ quickCheckWithResult args (p arg1 arg2)
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

data TestRunSettings = TestRunSettings
  { testRunSettingChildProcessOverride :: Maybe Bool, -- Nothing means use the default, the specific test can decide what that is.
    testRunSettingSeed :: Int,
    testRunSettingMaxSuccess :: Int,
    testRunSettingMaxSize :: Int,
    testRunSettingMaxDiscardRatio :: Int,
    testRunSettingMaxShrinks :: Int
  }
  deriving (Show, Generic)

defaultTestRunSettings :: TestRunSettings
defaultTestRunSettings =
  TestRunSettings
    { testRunSettingChildProcessOverride = Nothing,
      testRunSettingSeed = 42, -- This is set by default because we want reproducability by default.
      testRunSettingMaxSuccess = maxSuccess stdArgs,
      testRunSettingMaxSize = maxSize stdArgs,
      testRunSettingMaxDiscardRatio = maxDiscardRatio stdArgs,
      testRunSettingMaxShrinks = 100 -- This is different from what quickcheck does so that test suites are more likely to finish
    }

data TestRunResult = TestRunResult
  { testRunResultStatus :: !TestStatus,
    testRunResultException :: !(Maybe (Either String Assertion)),
    testRunResultNumTests :: !(Maybe Word),
    testRunResultNumShrinks :: !(Maybe Word),
    testRunResultExecutionTime :: !Double -- In seconds
  }
  deriving (Show, Eq, Generic)

data TestStatus = TestPassed | TestFailed
  deriving (Show, Eq, Generic)

instance NFData TestStatus

data Assertion
  = NotEqualButShouldHaveBeenEqual String String
  | EqualButShouldNotHaveBeenEqual String String
  | PredicateSucceededButShouldHaveFailed String
  | PredicateFailedButShouldHaveSucceeded String
  deriving (Show, Eq, Typeable, Generic)

instance Exception Assertion

instance NFData Assertion
