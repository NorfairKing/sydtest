{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Text.Show.Pretty
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
  let testRunResultGoldenCase = Nothing
  pure TestRunResult {..}

applyWrapper2 ::
  MonadIO m =>
  ((outerArgs -> innerArg -> m ()) -> m ()) ->
  (outerArgs -> innerArg -> m r) ->
  m r
applyWrapper2 wrapper func = do
  var <- liftIO newEmptyMVar
  wrapper $ \outerArgs innerArg -> do
    res <- func outerArgs innerArg
    liftIO $ putMVar var res
  liftIO $ readMVar var

instance IsTest (IO ()) where
  type Arg1 (IO ()) = ()
  type Arg2 (IO ()) = ()
  runTest func = runTest (\() () -> func)

instance IsTest (arg -> IO ()) where
  type Arg1 (arg -> IO ()) = ()
  type Arg2 (arg -> IO ()) = arg
  runTest func = runTest (\() -> func)

instance IsTest (outerArgs -> innerArg -> IO ()) where
  type Arg1 (outerArgs -> innerArg -> IO ()) = outerArgs
  type Arg2 (outerArgs -> innerArg -> IO ()) = innerArg
  runTest = runIOTestWithArg

runIOTestWithArg ::
  (outerArgs -> innerArg -> IO ()) ->
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
            (liftIO (func outerArgs innerArg) >>= (evaluate . (`seq` Right TestPassed)))
              `catches` exceptionHandlers
          evaluate $ case result of
            Left ex -> (TestFailed, Just ex)
            Right r -> (r, Nothing)
  let testRunResultNumShrinks = Nothing
  let testRunResultGoldenCase = Nothing
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
  result <-
    applyWrapper2 wrapper $ \outerArgs innerArg -> do
      quickCheckWithResult args (p outerArgs innerArg)
  let testRunResultStatus = case result of
        Success {} -> TestPassed
        GaveUp {} -> TestFailed
        Failure {} -> TestFailed
        NoExpectedFailure {} -> TestFailed
  let testRunResultNumTests = Just $ fromIntegral $ numTests result
  let testRunResultNumShrinks = case result of
        Failure {} -> Just $ fromIntegral $ numShrinks result
        _ -> Nothing
  let testRunResultGoldenCase = Nothing
  let testRunResultException = case result of
        Failure {} -> do
          se <- theException result
          pure $ case fromException se of
            Just a -> Right a
            Nothing -> Left $ displayException se
        _ -> Nothing
  pure TestRunResult {..}

data GoldenTest a = GoldenTest
  { goldenTestRead :: IO (Maybe a),
    goldenTestProduce :: IO a,
    goldenTestWrite :: a -> IO (),
    goldenTestCompare :: a -> a -> Bool
  }

instance Show a => IsTest (GoldenTest a) where
  type Arg1 (GoldenTest a) = ()
  type Arg2 (GoldenTest a) = ()
  runTest gt = runTest (\() () -> gt)

instance Show a => IsTest (arg -> GoldenTest a) where
  type Arg1 (arg -> GoldenTest a) = ()
  type Arg2 (arg -> GoldenTest a) = arg
  runTest gt = runTest (\() -> gt)

instance Show a => IsTest (outerArgs -> innerArg -> GoldenTest a) where
  type Arg1 (outerArgs -> innerArg -> GoldenTest a) = outerArgs
  type Arg2 (outerArgs -> innerArg -> GoldenTest a) = innerArg
  runTest func = runTest (\outerArgs innerArg -> pure (func outerArgs innerArg) :: IO (GoldenTest a))

instance Show a => IsTest (IO (GoldenTest a)) where
  type Arg1 (IO (GoldenTest a)) = ()
  type Arg2 (IO (GoldenTest a)) = ()
  runTest func = runTest (\() () -> func)

instance Show a => IsTest (arg -> IO (GoldenTest a)) where
  type Arg1 (arg -> IO (GoldenTest a)) = ()
  type Arg2 (arg -> IO (GoldenTest a)) = arg
  runTest func = runTest (\() -> func)

instance Show a => IsTest (outerArgs -> innerArg -> IO (GoldenTest a)) where
  type Arg1 (outerArgs -> innerArg -> IO (GoldenTest a)) = outerArgs
  type Arg2 (outerArgs -> innerArg -> IO (GoldenTest a)) = innerArg
  runTest = runGoldenTestWithArg

runGoldenTestWithArg ::
  Show a =>
  (outerArgs -> innerArg -> IO (GoldenTest a)) ->
  TestRunSettings ->
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  IO TestRunResult
runGoldenTestWithArg createGolden TestRunSettings {..} wrapper = do
  errOrTrip <- applyWrapper2 wrapper $ \outerArgs innerArgs -> ((`catches` exceptionHandlers) . fmap Right) $ do
    GoldenTest {..} <- createGolden outerArgs innerArgs
    mGolden <- goldenTestRead
    case mGolden of
      Nothing ->
        if testRunSettingGoldenStart
          then do
            actual <- goldenTestProduce
            goldenTestWrite actual
            pure (TestPassed, Just GoldenStarted, Nothing)
          else pure (TestFailed, Just GoldenNotFound, Nothing)
      Just golden -> do
        actual <- goldenTestProduce
        if goldenTestCompare actual golden
          then pure (TestPassed, Nothing, Nothing)
          else
            if testRunSettingGoldenReset
              then do
                goldenTestWrite actual
                pure (TestPassed, Just GoldenReset, Nothing)
              else pure (TestFailed, Nothing, Just $ Right $ NotEqualButShouldHaveBeenEqual (ppShow actual) (ppShow golden))
  let (testRunResultStatus, testRunResultGoldenCase, testRunResultException) = case errOrTrip of
        Left e -> (TestFailed, Nothing, Just e)
        Right trip -> trip
  let testRunResultNumTests = Nothing
  let testRunResultNumShrinks = Nothing
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
    testRunSettingMaxShrinks :: Int,
    testRunSettingGoldenStart :: Bool,
    testRunSettingGoldenReset :: Bool
  }
  deriving (Show, Generic)

defaultTestRunSettings :: TestRunSettings
defaultTestRunSettings =
  TestRunSettings
    { testRunSettingSeed = 42, -- This is set by default because we want reproducability by default.
      testRunSettingMaxSuccess = maxSuccess stdArgs,
      testRunSettingMaxSize = maxSize stdArgs,
      testRunSettingMaxDiscardRatio = maxDiscardRatio stdArgs,
      testRunSettingMaxShrinks = 100, -- This is different from what quickcheck does so that test suites are more likely to finish
      testRunSettingGoldenStart = True,
      testRunSettingGoldenReset = False
    }

data TestRunResult = TestRunResult
  { testRunResultStatus :: !TestStatus,
    testRunResultException :: !(Maybe (Either String Assertion)),
    testRunResultNumTests :: !(Maybe Word),
    testRunResultNumShrinks :: !(Maybe Word),
    testRunResultGoldenCase :: !(Maybe GoldenCase)
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

data GoldenCase
  = GoldenNotFound
  | GoldenStarted
  | GoldenReset
  deriving (Show, Eq, Typeable, Generic)

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
  deriving (Show, Eq, Generic, Functor)
