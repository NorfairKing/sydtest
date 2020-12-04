{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines all the functions you will use to define your test suite.
module Test.Syd.SpecDef where

import Data.Text (Text)
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Test.Syd.SpecForest

data TestDef v = TestDef {testDefVal :: v, testDefCallStack :: CallStack}
  deriving (Functor, Foldable, Traversable)

type TestForest a c = SpecDefForest a c ()

type TestTree a c = SpecDefTree a c ()

type SpecDefForest a c e = [SpecDefTree a c e]

data SpecDefTree a c e where -- a: input from 'aroundAll', c: input from 'around', e: extra
  DefDescribeNode :: Text -> SpecDefForest a c e -> SpecDefTree a c e -- A description
  DefSpecifyNode :: Text -> TestDef (((a -> c -> IO ()) -> IO ()) -> IO TestRunResult) -> e -> SpecDefTree a c e -- A test with its description
  DefAroundAllNode :: ((a -> IO ()) -> (b -> IO ())) -> SpecDefForest a c e -> SpecDefTree b c e
  DefParallelismNode :: Parallelism -> SpecDefForest a c e -> SpecDefTree a c e

instance Functor (SpecDefTree a c) where
  fmap f = \case
    DefDescribeNode t sf -> DefDescribeNode t (map (fmap f) sf)
    DefSpecifyNode t td e -> DefSpecifyNode t td (f e)
    DefAroundAllNode func sdf -> DefAroundAllNode func (map (fmap f) sdf)
    DefParallelismNode p sdf -> DefParallelismNode p (map (fmap f) sdf)

instance Foldable (SpecDefTree a c) where
  foldMap f = \case
    DefDescribeNode _ sf -> foldMap (foldMap f) sf
    DefSpecifyNode _ _ e -> f e
    DefAroundAllNode _ sdf -> foldMap (foldMap f) sdf
    DefParallelismNode _ sdf -> foldMap (foldMap f) sdf

instance Traversable (SpecDefTree a c) where
  traverse f = \case
    DefDescribeNode t sdf -> DefDescribeNode t <$> traverse (traverse f) sdf
    DefSpecifyNode t td e -> DefSpecifyNode t td <$> f e
    DefAroundAllNode func sdf -> DefAroundAllNode func <$> traverse (traverse f) sdf
    DefParallelismNode p sdf -> DefParallelismNode p <$> traverse (traverse f) sdf

data Parallelism = Parallel | Sequential

type ResultForest = SpecForest (TestDef TestRunResult)

type ResultTree = SpecTree (TestDef TestRunResult)

shouldExitFail :: ResultForest -> Bool
shouldExitFail = any (any ((== TestFailed) . testRunResultStatus . testDefVal))
