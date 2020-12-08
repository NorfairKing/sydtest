{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines the 'IsTest' class and the different instances for it.
module Test.Syd.Run where

import Control.Exception hiding (Handler, catches, evaluate)
import Data.Typeable
import Data.Word
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.IO ()
import Test.QuickCheck.Random
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
  type Arg1 Bool = () -- The argument from 'aroundAll'
  type Arg2 Bool = () -- The argument from 'around'
  runTest func = runTest (\() () -> func)

instance IsTest (arg -> Bool) where
  type Arg1 (arg -> Bool) = ()
  type Arg2 (arg -> Bool) = arg
  runTest func = runTest (\() arg -> func arg)

instance IsTest (outerArgs -> innerArg -> Bool) where
  type Arg1 (outerArgs -> innerArg -> Bool) = outerArgs
  type Arg2 (outerArgs -> innerArg -> Bool) = innerArg
  runTest = runPureTestWithArg

runPureTestWithArg ::
  (outerArgs -> innerArg -> Bool) ->
  TestRunSettings ->
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  IO TestRunResult
runPureTestWithArg computeBool TestRunSettings {..} wrapper = do
  let testRunResultNumTests = Nothing
  resultBool <-
    liftIO $
      applyWrapper2 wrapper $
        \outerArgs innerArg -> (Right <$> evaluate (computeBool outerArgs innerArg)) `catches` exceptionHandlers
  let (testRunResultStatus, testRunResultException) = case resultBool of
        Left ex -> (TestFailed, Just ex)
        Right bool -> (if bool then TestPassed else TestFailed, Nothing)
  let testRunResultNumShrinks = Nothing
  pure TestRunResult {..}

applyWrapper2 ::
  MonadIO m =>
  ((outerArgs -> innerArg -> m ()) -> m ()) ->
  (outerArgs -> innerArg -> m r) ->
  m r
applyWrapper2 wrapper func = do
  var <- liftIO $ newEmptyMVar
  wrapper $ \outerArgs innerArg -> do
    res <- func outerArgs innerArg
    liftIO $ putMVar var res
  liftIO $ readMVar var

instance IsTest (IO a) where
  type Arg1 (IO a) = ()
  type Arg2 (IO a) = ()
  runTest func = runTest (\() () -> func)

instance IsTest (arg -> IO a) where
  type Arg1 (arg -> IO a) = ()
  type Arg2 (arg -> IO a) = arg
  runTest func = runTest (\() -> func)

instance IsTest (outerArgs -> innerArg -> IO a) where
  type Arg1 (outerArgs -> innerArg -> IO a) = outerArgs
  type Arg2 (outerArgs -> innerArg -> IO a) = innerArg
  runTest = runIOTestWithArg

runIOTestWithArg ::
  (outerArgs -> innerArg -> IO a) ->
  TestRunSettings ->
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  IO TestRunResult
runIOTestWithArg func TestRunSettings {..} wrapper = do
  let testRunResultNumTests = Nothing
  (testRunResultStatus, testRunResultException) <-
    liftIO $
      applyWrapper2 wrapper $
        \outerArgs innerArg -> do
          result <-
            (liftIO (() <$ func outerArgs innerArg) >>= (evaluate . (`seq` Right TestPassed)))
              `catches` exceptionHandlers
          evaluate $ case result of
            Left ex -> (TestFailed, Just ex)
            Right r -> (r, Nothing)
  let testRunResultNumShrinks = Nothing
  pure TestRunResult {..}

instance IsTest Property where
  type Arg1 Property = ()
  type Arg2 Property = ()
  runTest func = runTest (\() () -> func)

instance IsTest (arg -> Property) where
  type Arg1 (arg -> Property) = ()
  type Arg2 (arg -> Property) = arg
  runTest func = runTest (\() -> func)

instance IsTest (outerArgs -> innerArg -> Property) where
  type Arg1 (outerArgs -> innerArg -> Property) = outerArgs
  type Arg2 (outerArgs -> innerArg -> Property) = innerArg
  runTest = runPropertyTestWithArg

runPropertyTestWithArg ::
  (outerArgs -> innerArg -> Property) ->
  TestRunSettings ->
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
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
  liftIO $ do
    result <-
      applyWrapper2 wrapper $ \outerArgs innerArg -> do
        liftIO $ quickCheckWithResult args (p outerArgs innerArg)
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
  { testRunSettingSeed :: Int,
    testRunSettingMaxSuccess :: Int,
    testRunSettingMaxSize :: Int,
    testRunSettingMaxDiscardRatio :: Int,
    testRunSettingMaxShrinks :: Int
  }
  deriving (Show, Generic)

defaultTestRunSettings :: TestRunSettings
defaultTestRunSettings =
  TestRunSettings
    { testRunSettingSeed = 42, -- This is set by default because we want reproducability by default.
      testRunSettingMaxSuccess = maxSuccess stdArgs,
      testRunSettingMaxSize = maxSize stdArgs,
      testRunSettingMaxDiscardRatio = maxDiscardRatio stdArgs,
      testRunSettingMaxShrinks = 100 -- This is different from what quickcheck does so that test suites are more likely to finish
    }

data TestRunResult = TestRunResult
  { testRunResultStatus :: !TestStatus,
    testRunResultException :: !(Maybe (Either String Assertion)),
    testRunResultNumTests :: !(Maybe Word),
    testRunResultNumShrinks :: !(Maybe Word)
  }
  deriving (Show, Eq, Generic)

data TestStatus = TestPassed | TestFailed
  deriving (Show, Eq, Generic)

data Assertion
  = NotEqualButShouldHaveBeenEqual String String
  | EqualButShouldNotHaveBeenEqual String String
  | PredicateSucceededButShouldHaveFailed String
  | PredicateFailedButShouldHaveSucceeded String
  deriving (Show, Eq, Typeable, Generic)

instance Exception Assertion

-- | Time an action and return the result as well as how long it took in seconds.
--
-- This function does not use the 'timeit' package because that package uses CPU time instead of system time.
-- That means that any waiting, like with 'threadDelay' would not be counted.
--
-- Note that this does not evaluate the result, on purpose.
timeItT :: MonadIO m => m a -> m (Timed a)
timeItT func = do
  begin <- liftIO getMonotonicTimeNSec
  r <- func
  end <- liftIO getMonotonicTimeNSec
  pure $ Timed r (end - begin)

data Timed a = Timed
  { timedValue :: !a,
    -- | In nanoseconds
    timedTime :: !Word64
  }
  deriving (Show, Eq, Generic)
