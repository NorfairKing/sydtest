{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
  DefAroundAllNode ::
    -- ( HContains (HList a l) a,
    --   HContains (HList b (a ': l)) a,
    --   HContains (HList b (a ': l)) b
    -- ) =>
    ((b -> IO ()) -> (a -> IO ())) ->
    SpecDefForest (HList b (a ': l)) c e ->
    SpecDefTree (HList a l) c e
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

data HList h (r :: [*]) where
  HLast :: h -> HList h '[]
  HCons :: e -> HList h l -> HList e (h ': l)

class HLast a where
  type LastElem a :: *
  getLastElem :: a -> LastElem a

instance HLast (HList h '[]) where
  type LastElem (HList h '[]) = h
  getLastElem = \case
    HLast e -> e

instance HLast (HList a b) => HLast (HList h (a ': b)) where
  type LastElem (HList h (a ': b)) = LastElem (HList a b)
  getLastElem = \case
    HCons _ hl -> getLastElem hl

class HContains a b where
  getElem :: a -> b

instance HContains (HList a l) a where
  getElem (HLast a) = a
  getElem (HCons a _) = a

instance HContains (HList a b) a => HContains (HList h (a ': b)) a where
  getElem (HCons _ hl) = getElem hl

instance HContains (HList x l) a => HContains (HList b (x ': l)) a where
  getElem (HCons _ hl) = getElem hl

instance {-# OVERLAPPING #-} HContains a a where
  getElem = id
