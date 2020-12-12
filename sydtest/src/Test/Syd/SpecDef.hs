{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines all the functions you will use to define your test suite.
module Test.Syd.SpecDef where

import Data.Kind
import Data.Text (Text)
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.HList
import Test.Syd.Run
import Test.Syd.SpecForest

data TestDef v = TestDef {testDefVal :: v, testDefCallStack :: CallStack}
  deriving (Functor, Foldable, Traversable)

type TestForest a c = SpecDefForest a c ()

type TestTree a c = SpecDefTree a c ()

type SpecDefForest (a :: [Type]) c e = [SpecDefTree a c e]

data SpecDefTree (a :: [Type]) c e where -- a: input from 'aroundAll', c: input from 'around', e: extra
  DefSpecifyNode ::
    Text ->
    TestDef (((HList a -> c -> IO ()) -> IO ()) -> IO TestRunResult) ->
    e ->
    SpecDefTree a c e -- A test with its description
  DefPendingNode :: Text -> Maybe Text -> SpecDefTree a c e
  DefDescribeNode :: Text -> SpecDefForest a c e -> SpecDefTree a c e -- A description
  DefWrapNode :: (IO () -> IO ()) -> SpecDefForest a c e -> SpecDefTree a c e
  DefBeforeAllNode :: IO a -> SpecDefForest (a ': l) c e -> SpecDefTree l c e
  DefAroundAllNode ::
    ((a -> IO ()) -> IO ()) ->
    SpecDefForest (a ': l) c e ->
    SpecDefTree l c e
  DefAroundAllWithNode ::
    ((b -> IO ()) -> (a -> IO ())) ->
    SpecDefForest (b ': a ': l) c e ->
    SpecDefTree (a ': l) c e
  DefAfterAllNode :: (HList a -> IO ()) -> SpecDefForest a c e -> SpecDefTree a c e
  DefParallelismNode :: Parallelism -> SpecDefForest a c e -> SpecDefTree a c e
  DefRandomisationNode :: ExecutionOrderRandomisation -> SpecDefForest a c e -> SpecDefTree a c e

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

data Parallelism = Parallel | Sequential

data ExecutionOrderRandomisation = RandomiseExecutionOrder | DoNotRandomiseExecutionOrder

type ResultForest = SpecForest (TestDef (Timed TestRunResult))

type ResultTree = SpecTree (TestDef (Timed TestRunResult))

computeTestSuiteStats :: ResultForest -> TestSuiteStats
computeTestSuiteStats = goF
  where
    goF :: ResultForest -> TestSuiteStats
    goF = foldMap goT
    goT :: ResultTree -> TestSuiteStats
    goT = \case
      SpecifyNode _ (TestDef (Timed TestRunResult {..} _) _) ->
        TestSuiteStats
          { testSuiteStatSuccesses = case testRunResultStatus of
              TestPassed -> 1
              TestFailed -> 0,
            testSuiteStatFailures = case testRunResultStatus of
              TestPassed -> 0
              TestFailed -> 1,
            testSuiteStatPending = 0
          }
      PendingNode _ _ ->
        TestSuiteStats
          { testSuiteStatSuccesses = 0,
            testSuiteStatFailures = 0,
            testSuiteStatPending = 1
          }
      DescribeNode _ sf -> goF sf
      SubForestNode sf -> goF sf

data TestSuiteStats = TestSuiteStats
  { testSuiteStatSuccesses :: !Word,
    testSuiteStatFailures :: !Word,
    testSuiteStatPending :: !Word
  }
  deriving (Show, Eq)

instance Semigroup TestSuiteStats where
  (<>) tss1 tss2 =
    TestSuiteStats
      { testSuiteStatSuccesses = testSuiteStatSuccesses tss1 + testSuiteStatSuccesses tss2,
        testSuiteStatFailures = testSuiteStatFailures tss1 + testSuiteStatFailures tss2,
        testSuiteStatPending = testSuiteStatPending tss1 + testSuiteStatPending tss2
      }

instance Monoid TestSuiteStats where
  mappend = (<>)
  mempty =
    TestSuiteStats
      { testSuiteStatSuccesses = 0,
        testSuiteStatFailures = 0,
        testSuiteStatPending = 0
      }

shouldExitFail :: ResultForest -> Bool
shouldExitFail = any (any ((== TestFailed) . testRunResultStatus . timedValue . testDefVal))
