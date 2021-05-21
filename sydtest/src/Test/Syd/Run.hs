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

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Typeable
import Data.Word
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.IO ()
import Test.QuickCheck.Property hiding (Result (..))
import qualified Test.QuickCheck.Property as QCP
import Test.QuickCheck.Random
import Test.Syd.Run.Property
import Test.Syd.Run.Result
import Test.Syd.Run.Settings
import Text.Printf

class IsTest e where
  -- | The argument from 'aroundAll'
  type Arg1 e

  -- | The argument from 'around'
  type Arg2 e

  -- | Running the test, safely
  runTest ::
    e ->
    TestRunSettings ->
    ((Arg1 e -> Arg2 e -> IO ()) -> IO ()) ->
    IO TestRunResult

instance IsTest Bool where
  type Arg1 Bool = ()
  type Arg2 Bool = ()
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
runPureTestWithArg computeBool TestRunSettings {} wrapper = do
  let testRunResultNumTests = Nothing
  resultBool <-
    applyWrapper2 wrapper $
      \outerArgs innerArg -> evaluate (computeBool outerArgs innerArg)
  let (testRunResultStatus, testRunResultException) = case resultBool of
        Left ex -> (TestFailed, Just ex)
        Right bool -> (if bool then TestPassed else TestFailed, Nothing)
  let testRunResultNumShrinks = Nothing
  let testRunResultGoldenCase = Nothing
  let testRunResultFailingInputs = []
  let testRunResultExtraInfo = Nothing
  let testRunResultLabels = Nothing
  let testRunResultClasses = Nothing
  let testRunResultTables = Nothing
  pure TestRunResult {..}

applyWrapper2 ::
  forall r outerArgs innerArg.
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  (outerArgs -> innerArg -> IO r) ->
  IO (Either (Either String Assertion) r)
applyWrapper2 wrapper func = do
  var <- liftIO newEmptyMVar
  r <- (`catches` exceptionHandlers) $
    fmap Right $
      wrapper $ \outerArgs innerArg -> do
        res <- (Right <$> (func outerArgs innerArg >>= evaluate)) `catches` exceptionHandlers
        liftIO $ putMVar var res
  case r of
    Right () -> liftIO $ readMVar var
    Left e -> pure (Left e :: Either (Either String Assertion) r)

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
runIOTestWithArg func TestRunSettings {} wrapper = do
  let testRunResultNumTests = Nothing
  result <- liftIO $
    applyWrapper2 wrapper $
      \outerArgs innerArg ->
        func outerArgs innerArg >>= evaluate
  let (testRunResultStatus, testRunResultException) = case result of
        Left ex -> (TestFailed, Just ex)
        Right () -> (TestPassed, Nothing)
  let testRunResultNumShrinks = Nothing
  let testRunResultGoldenCase = Nothing
  let testRunResultFailingInputs = []
  let testRunResultExtraInfo = Nothing
  let testRunResultLabels = Nothing
  let testRunResultClasses = Nothing
  let testRunResultTables = Nothing
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

-- | A golden test for output of type @a@.
--
-- The purpose of a golden test is to ensure that the output of a certain
-- process does not change even over time.
--
-- Golden tests can also be used to show how the output of a certain process
-- changes over time and force code reviewers to review the diff that they see
-- in the PR.
--
-- This works by saving a 'golden' output in the repository somewhere,
-- committing it, and then compare that golden output to the output that is
-- currently being produced. You can use `--golden-reset` to have sydtest
-- update the golden output by writing the current output.
data GoldenTest a = GoldenTest
  { -- | Read the golden test output, 'Nothing' if there is no golden output yet.
    goldenTestRead :: IO (Maybe a),
    -- | Produce the current output
    goldenTestProduce :: IO a,
    -- | Write golden output
    goldenTestWrite :: a -> IO (),
    -- | Compare golden output with current output
    --
    -- The first argument is the current output, the second is the golden output
    goldenTestCompare :: a -> a -> Maybe Assertion
  }

instance IsTest (GoldenTest a) where
  type Arg1 (GoldenTest a) = ()
  type Arg2 (GoldenTest a) = ()
  runTest gt = runTest (\() () -> gt)

instance IsTest (arg -> GoldenTest a) where
  type Arg1 (arg -> GoldenTest a) = ()
  type Arg2 (arg -> GoldenTest a) = arg
  runTest gt = runTest (\() -> gt)

instance IsTest (outerArgs -> innerArg -> GoldenTest a) where
  type Arg1 (outerArgs -> innerArg -> GoldenTest a) = outerArgs
  type Arg2 (outerArgs -> innerArg -> GoldenTest a) = innerArg
  runTest func = runTest (\outerArgs innerArg -> pure (func outerArgs innerArg) :: IO (GoldenTest a))

instance IsTest (IO (GoldenTest a)) where
  type Arg1 (IO (GoldenTest a)) = ()
  type Arg2 (IO (GoldenTest a)) = ()
  runTest func = runTest (\() () -> func)

instance IsTest (arg -> IO (GoldenTest a)) where
  type Arg1 (arg -> IO (GoldenTest a)) = ()
  type Arg2 (arg -> IO (GoldenTest a)) = arg
  runTest func = runTest (\() -> func)

instance IsTest (outerArgs -> innerArg -> IO (GoldenTest a)) where
  type Arg1 (outerArgs -> innerArg -> IO (GoldenTest a)) = outerArgs
  type Arg2 (outerArgs -> innerArg -> IO (GoldenTest a)) = innerArg
  runTest = runGoldenTestWithArg

runGoldenTestWithArg ::
  (outerArgs -> innerArg -> IO (GoldenTest a)) ->
  TestRunSettings ->
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  IO TestRunResult
runGoldenTestWithArg createGolden TestRunSettings {..} wrapper = do
  errOrTrip <- applyWrapper2 wrapper $ \outerArgs innerArgs -> do
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
        case goldenTestCompare actual golden of
          Nothing -> pure (TestPassed, Nothing, Nothing)
          Just assertion ->
            if testRunSettingGoldenReset
              then do
                goldenTestWrite actual
                pure (TestPassed, Just GoldenReset, Nothing)
              else pure (TestFailed, Nothing, Just $ Right assertion)
  let (testRunResultStatus, testRunResultGoldenCase, testRunResultException) = case errOrTrip of
        Left e -> (TestFailed, Nothing, Just e)
        Right trip -> trip
  let testRunResultNumTests = Nothing
  let testRunResultNumShrinks = Nothing
  let testRunResultFailingInputs = []
  let testRunResultExtraInfo = Nothing
  let testRunResultLabels = Nothing
  let testRunResultClasses = Nothing
  let testRunResultTables = Nothing
  pure TestRunResult {..}

exceptionHandlers :: [Handler (Either (Either String Assertion) a)]
exceptionHandlers =
  [ -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT (ctrl-c).
    Handler (\e -> throw (e :: AsyncException)),
    -- Catch assertions first because we know what to do with them.
    Handler $ \(a :: Assertion) -> pure (Left $ Right a),
    -- Catch all the rest as a string
    Handler (\e -> return $ Left (Left (displayException (e :: SomeException))))
  ]

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
