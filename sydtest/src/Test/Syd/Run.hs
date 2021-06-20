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
import Control.Monad.Reader
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

instance IsTest (ReaderT env IO ()) where
  type Arg1 (ReaderT env IO ()) = ()
  type Arg2 (ReaderT env IO ()) = env
  runTest func = runTest (\() -> func)

instance IsTest (outerArgs -> ReaderT env IO ()) where
  type Arg1 (outerArgs -> ReaderT env IO ()) = outerArgs
  type Arg2 (outerArgs -> ReaderT env IO ()) = env
  runTest func = runTest (\outerArgs env -> runReaderT (func outerArgs) env)

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
  runTest func = runTest (\() -> func)

instance IsTest (arg -> Property) where
  type Arg1 (arg -> Property) = ()
  type Arg2 (arg -> Property) = arg
  runTest func = runTest (\() -> func)

instance IsTest (outerArgs -> innerArg -> Property) where
  type Arg1 (outerArgs -> innerArg -> Property) = outerArgs
  type Arg2 (outerArgs -> innerArg -> Property) = innerArg
  runTest = runPropertyTestWithArg

makeQuickCheckArgs :: TestRunSettings -> Args
makeQuickCheckArgs TestRunSettings {..} =
  stdArgs
    { replay = Just (mkQCGen testRunSettingSeed, 0),
      chatty = False,
      maxSuccess = testRunSettingMaxSuccess,
      maxDiscardRatio = testRunSettingMaxDiscardRatio,
      maxSize = testRunSettingMaxSize,
      maxShrinks = testRunSettingMaxShrinks
    }

runPropertyTestWithArg ::
  (outerArgs -> innerArg -> Property) ->
  TestRunSettings ->
  ((outerArgs -> innerArg -> IO ()) -> IO ()) ->
  IO TestRunResult
runPropertyTestWithArg p trs wrapper = do
  let qcargs = makeQuickCheckArgs trs
  qcr <- quickCheckWithResult qcargs (aroundProperty wrapper p)
  let testRunResultGoldenCase = Nothing
  let testRunResultNumTests = Just $ fromIntegral $ numTests qcr
  case qcr of
    Success {} -> do
      let testRunResultStatus = TestPassed
      let testRunResultException = Nothing
      let testRunResultNumShrinks = Nothing
      let testRunResultFailingInputs = []
      let testRunResultExtraInfo = Nothing
      let testRunResultLabels = Just $ labels qcr
      let testRunResultClasses = Just $ classes qcr
      let testRunResultTables = Just $ tables qcr
      pure TestRunResult {..}
    GaveUp {} -> do
      let testRunResultStatus = TestFailed
      let testRunResultException = Nothing
      let testRunResultNumShrinks = Nothing
      let testRunResultFailingInputs = []
      let testRunResultExtraInfo = Just $ printf "Gave up, %d discarded tests" (numDiscarded qcr)
      let testRunResultLabels = Just $ labels qcr
      let testRunResultClasses = Just $ classes qcr
      let testRunResultTables = Just $ tables qcr
      pure TestRunResult {..}
    Failure {} -> do
      let testRunResultStatus = TestFailed
      let testRunResultException = do
            se <- theException qcr
            pure $ case fromException se of
              Just a -> Right a
              Nothing -> Left $ displayException se
      let testRunResultNumShrinks = Just $ fromIntegral $ numShrinks qcr
      let testRunResultFailingInputs = failingTestCase qcr
      let testRunResultExtraInfo = Nothing
      let testRunResultLabels = Just $ M.singleton (failingLabels qcr) 1
      let testRunResultClasses = Just $ M.fromSet (const 1) (failingClasses qcr)
      let testRunResultTables = Nothing
      pure TestRunResult {..}
    NoExpectedFailure {} -> do
      let testRunResultStatus = TestFailed
      let testRunResultException = Nothing
      let testRunResultNumShrinks = Nothing
      let testRunResultFailingInputs = []
      let testRunResultLabels = Just $ labels qcr
      let testRunResultClasses = Just $ classes qcr
      let testRunResultTables = Just $ tables qcr
      let testRunResultExtraInfo = Just $ printf "Expected the property to fail but it didn't."
      pure TestRunResult {..}

aroundProperty :: ((a -> b -> IO ()) -> IO ()) -> (a -> b -> Property) -> Property
aroundProperty action p = MkProperty . MkGen $ \r n -> aroundProp action $ \a b -> (unGen . unProperty $ p a b) r n

aroundProp :: ((a -> b -> IO ()) -> IO ()) -> (a -> b -> Prop) -> Prop
aroundProp action p = MkProp $ aroundRose action (\a b -> unProp $ p a b)

aroundRose :: ((a -> b -> IO ()) -> IO ()) -> (a -> b -> Rose QCP.Result) -> Rose QCP.Result
aroundRose action r = ioRose $ do
  ref <- newIORef (return QCP.succeeded)
  action $ \a b -> reduceRose (r a b) >>= writeIORef ref
  readIORef ref

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

type Test = IO ()

data TestRunSettings = TestRunSettings
  { testRunSettingSeed :: !Int,
    testRunSettingMaxSuccess :: !Int,
    testRunSettingMaxSize :: !Int,
    testRunSettingMaxDiscardRatio :: !Int,
    testRunSettingMaxShrinks :: !Int,
    testRunSettingGoldenStart :: !Bool,
    testRunSettingGoldenReset :: !Bool
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
    testRunResultFailingInputs :: [String],
    testRunResultLabels :: !(Maybe (Map [String] Int)),
    testRunResultClasses :: !(Maybe (Map String Int)),
    testRunResultTables :: !(Maybe (Map String (Map String Int))),
    testRunResultGoldenCase :: !(Maybe GoldenCase),
    testRunResultExtraInfo :: !(Maybe String)
  }
  deriving (Show, Eq, Generic)

data TestStatus = TestPassed | TestFailed
  deriving (Show, Eq, Generic)

-- | A special exception that sydtest knows about and can display nicely in the error output
--
-- This is exported outwards so that you can define golden tests for custom types.
--
-- You will probably not want to use this directly in everyday tests, use `shouldBe` or a similar function instead.
data Assertion
  = NotEqualButShouldHaveBeenEqual String String
  | EqualButShouldNotHaveBeenEqual String String
  | PredicateSucceededButShouldHaveFailed
      String -- Value
      (Maybe String) -- Name of the predicate
  | PredicateFailedButShouldHaveSucceeded
      String -- Value
      (Maybe String) -- Name of the predicate
  | ExpectationFailed String
  | Context Assertion String
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
