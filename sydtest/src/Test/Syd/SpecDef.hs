{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines all the functions you will use to define your test suite.
module Test.Syd.SpecDef where

import Data.Kind
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.HList
import Test.Syd.Run
import Test.Syd.SpecForest

data TDef value = TDef {testDefVal :: value, testDefCallStack :: CallStack}
  deriving (Functor, Foldable, Traversable)

type TestForest outers inner = SpecDefForest outers inner ()

type TestTree outers inner = SpecDefTree outers inner ()

type SpecDefForest (outers :: [Type]) inner extra = [SpecDefTree outers inner extra]

-- | A tree of tests
--
-- This type has three parameters:
--
-- * @outers@: A type-level list of the outer resources. These are resources that are prived once, around a group of tests. (This is the type of the results of `aroundAll`.)
-- * @inner@: The inner resource. This is a resource that is set up around every test, and even every example of a property test. (This is the type of the result of `around`.)
-- * @result@: The result ('TestDefM' is a monad.)
--
-- In practice, all of these three parameters should be '()' at the top level.
--
-- When you're just using sydtest and not writing a library for sydtest, you probably don't even want to concern yourself with this type.
data SpecDefTree (outers :: [Type]) inner extra where
  -- | Define a test
  DefSpecifyNode ::
    -- | The description of the test
    Text ->
    -- | How the test can be run given a function that provides the resources
    TDef (((HList outers -> inner -> IO ()) -> IO ()) -> IO TestRunResult) ->
    extra ->
    SpecDefTree outers inner extra
  -- | Define a pending test
  DefPendingNode ::
    -- | The description of the test
    Text ->
    -- | The reason why the test is pending
    Maybe Text ->
    SpecDefTree outers inner extra
  -- | Group tests using a description
  DefDescribeNode ::
    -- | The description
    Text ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  DefWrapNode ::
    -- | The function that wraps running the tests.
    (IO () -> IO ()) ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  DefBeforeAllNode ::
    -- | The function to run (once), beforehand, to produce the outer resource.
    IO outer ->
    SpecDefForest (outer ': otherOuters) inner extra ->
    SpecDefTree otherOuters inner extra
  DefAroundAllNode ::
    -- | The function that provides the outer resource (once), around the tests.
    ((outer -> IO ()) -> IO ()) ->
    SpecDefForest (outer ': otherOuters) inner extra ->
    SpecDefTree otherOuters inner extra
  DefAroundAllWithNode ::
    -- | The function that provides the new outer resource (once), using the old outer resource.
    ((newOuter -> IO ()) -> (oldOuter -> IO ())) ->
    SpecDefForest (newOuter ': oldOuter ': otherOuters) inner extra ->
    SpecDefTree (oldOuter ': otherOuters) inner extra
  DefAfterAllNode ::
    -- | The function to run (once), afterwards, using all outer resources.
    (HList outers -> IO ()) ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  -- | Control the level of parallelism for a given group of tests
  DefParallelismNode ::
    -- | The level of parallelism
    Parallelism ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  -- | Control the execution order randomisation for a given group of tests
  DefRandomisationNode ::
    -- | The execution order randomisation
    ExecutionOrderRandomisation ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra
  DefFlakinessNode ::
    -- | How many times to retry
    FlakinessMode ->
    SpecDefForest outers inner extra ->
    SpecDefTree outers inner extra

instance Functor (SpecDefTree a c) where
  fmap :: forall e f. (e -> f) -> SpecDefTree a c e -> SpecDefTree a c f
  fmap f =
    let goF :: forall x y. SpecDefForest x y e -> SpecDefForest x y f
        goF = map (fmap f)
     in \case
          DefDescribeNode t sdf -> DefDescribeNode t $ goF sdf
          DefPendingNode t mr -> DefPendingNode t mr
          DefSpecifyNode t td e -> DefSpecifyNode t td (f e)
          DefWrapNode func sdf -> DefWrapNode func $ goF sdf
          DefBeforeAllNode func sdf -> DefBeforeAllNode func $ goF sdf
          DefAroundAllNode func sdf -> DefAroundAllNode func $ goF sdf
          DefAroundAllWithNode func sdf -> DefAroundAllWithNode func $ goF sdf
          DefAfterAllNode func sdf -> DefAfterAllNode func $ goF sdf
          DefParallelismNode p sdf -> DefParallelismNode p $ goF sdf
          DefRandomisationNode p sdf -> DefRandomisationNode p $ goF sdf
          DefFlakinessNode p sdf -> DefFlakinessNode p $ goF sdf

instance Foldable (SpecDefTree a c) where
  foldMap :: forall e m. Monoid m => (e -> m) -> SpecDefTree a c e -> m
  foldMap f =
    let goF :: forall x y. SpecDefForest x y e -> m
        goF = foldMap (foldMap f)
     in \case
          DefDescribeNode _ sdf -> goF sdf
          DefPendingNode _ _ -> mempty
          DefSpecifyNode _ _ e -> f e
          DefWrapNode _ sdf -> goF sdf
          DefBeforeAllNode _ sdf -> goF sdf
          DefAroundAllNode _ sdf -> goF sdf
          DefAroundAllWithNode _ sdf -> goF sdf
          DefAfterAllNode _ sdf -> goF sdf
          DefParallelismNode _ sdf -> goF sdf
          DefRandomisationNode _ sdf -> goF sdf
          DefFlakinessNode _ sdf -> goF sdf

instance Traversable (SpecDefTree a c) where
  traverse :: forall u w f. Applicative f => (u -> f w) -> SpecDefTree a c u -> f (SpecDefTree a c w)
  traverse f =
    let goF :: forall x y. SpecDefForest x y u -> f (SpecDefForest x y w)
        goF = traverse (traverse f)
     in \case
          DefDescribeNode t sdf -> DefDescribeNode t <$> goF sdf
          DefPendingNode t mr -> pure $ DefPendingNode t mr
          DefSpecifyNode t td e -> DefSpecifyNode t td <$> f e
          DefWrapNode func sdf -> DefWrapNode func <$> goF sdf
          DefBeforeAllNode func sdf -> DefBeforeAllNode func <$> goF sdf
          DefAroundAllNode func sdf -> DefAroundAllNode func <$> goF sdf
          DefAroundAllWithNode func sdf -> DefAroundAllWithNode func <$> goF sdf
          DefAfterAllNode func sdf -> DefAfterAllNode func <$> goF sdf
          DefParallelismNode p sdf -> DefParallelismNode p <$> goF sdf
          DefRandomisationNode p sdf -> DefRandomisationNode p <$> goF sdf
          DefFlakinessNode p sdf -> DefFlakinessNode p <$> goF sdf

data Parallelism = Parallel | Sequential

data ExecutionOrderRandomisation = RandomiseExecutionOrder | DoNotRandomiseExecutionOrder

data FlakinessMode = MayNotBeFlaky | MayBeFlakyUpTo !Int

type ResultForest = SpecForest (TDef (Timed TestRunResult))

type ResultTree = SpecTree (TDef (Timed TestRunResult))

computeTestSuiteStats :: ResultForest -> TestSuiteStats
computeTestSuiteStats = goF []
  where
    goF :: [Text] -> ResultForest -> TestSuiteStats
    goF ts = foldMap (goT ts)
    goT :: [Text] -> ResultTree -> TestSuiteStats
    goT ts = \case
      SpecifyNode tn (TDef (Timed TestRunResult {..} t) _) ->
        TestSuiteStats
          { testSuiteStatSuccesses = case testRunResultStatus of
              TestPassed -> 1
              TestFailed -> 0,
            testSuiteStatExamples = case testRunResultStatus of
              TestPassed -> fromMaybe 1 testRunResultNumTests
              TestFailed -> fromMaybe 1 testRunResultNumTests + fromMaybe 0 testRunResultNumShrinks,
            testSuiteStatFailures = case testRunResultStatus of
              TestPassed -> 0
              TestFailed -> 1,
            testSuiteStatPending = 0,
            testSuiteStatSumTime = t,
            testSuiteStatLongestTime = Just (T.intercalate "." (ts ++ [tn]), t)
          }
      PendingNode _ _ ->
        TestSuiteStats
          { testSuiteStatSuccesses = 0,
            testSuiteStatExamples = 0,
            testSuiteStatFailures = 0,
            testSuiteStatPending = 1,
            testSuiteStatSumTime = 0,
            testSuiteStatLongestTime = Nothing
          }
      DescribeNode t sf -> goF (t : ts) sf
      SubForestNode sf -> goF ts sf

data TestSuiteStats = TestSuiteStats
  { testSuiteStatSuccesses :: !Word,
    testSuiteStatExamples :: !Word,
    testSuiteStatFailures :: !Word,
    testSuiteStatPending :: !Word,
    testSuiteStatSumTime :: !Word64,
    testSuiteStatLongestTime :: !(Maybe (Text, Word64))
  }
  deriving (Show, Eq)

instance Semigroup TestSuiteStats where
  (<>) tss1 tss2 =
    TestSuiteStats
      { testSuiteStatSuccesses = testSuiteStatSuccesses tss1 + testSuiteStatSuccesses tss2,
        testSuiteStatExamples = testSuiteStatExamples tss1 + testSuiteStatExamples tss2,
        testSuiteStatFailures = testSuiteStatFailures tss1 + testSuiteStatFailures tss2,
        testSuiteStatPending = testSuiteStatPending tss1 + testSuiteStatPending tss2,
        testSuiteStatSumTime = testSuiteStatSumTime tss1 + testSuiteStatSumTime tss2,
        testSuiteStatLongestTime = case (testSuiteStatLongestTime tss1, testSuiteStatLongestTime tss2) of
          (Nothing, Nothing) -> Nothing
          (Just t1, Nothing) -> Just t1
          (Nothing, Just t2) -> Just t2
          (Just (tn1, t1), Just (tn2, t2)) -> Just $ if t1 >= t2 then (tn1, t1) else (tn2, t2)
      }

instance Monoid TestSuiteStats where
  mappend = (<>)
  mempty =
    TestSuiteStats
      { testSuiteStatSuccesses = 0,
        testSuiteStatExamples = 0,
        testSuiteStatFailures = 0,
        testSuiteStatPending = 0,
        testSuiteStatSumTime = 0,
        testSuiteStatLongestTime = Nothing
      }

shouldExitFail :: ResultForest -> Bool
shouldExitFail = any (any ((== TestFailed) . testRunResultStatus . timedValue . testDefVal))
