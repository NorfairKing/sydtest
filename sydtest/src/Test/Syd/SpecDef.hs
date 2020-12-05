{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
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
  DefSpecifyNode :: Text -> TestDef (((a -> c -> IO ()) -> IO ()) -> IO TestRunResult) -> e -> SpecDefTree a c e -- A test with its description
  DefDescribeNode :: Text -> SpecDefForest a c e -> SpecDefTree a c e -- A description
  DefWrapNode :: (IO () -> IO ()) -> SpecDefForest a c e -> SpecDefTree a c e
  DefAroundAllWithNode ::
    ((b -> IO ()) -> (a -> IO ())) ->
    SpecDefForest (HList b (a ': l)) c e ->
    SpecDefTree (HList a l) c e
  DefAfterAllNode :: (a -> IO ()) -> SpecDefForest a c e -> SpecDefTree a c e
  DefParallelismNode :: Parallelism -> SpecDefForest a c e -> SpecDefTree a c e

instance Functor (SpecDefTree a c) where
  fmap :: forall e f. (e -> f) -> SpecDefTree a c e -> SpecDefTree a c f
  fmap f =
    let goF :: forall x y. SpecDefForest x y e -> SpecDefForest x y f
        goF = map (fmap f)
     in \case
          DefDescribeNode t sdf -> DefDescribeNode t $ goF sdf
          DefSpecifyNode t td e -> DefSpecifyNode t td (f e)
          DefWrapNode func sdf -> DefWrapNode func $ goF sdf
          DefAroundAllWithNode func sdf -> DefAroundAllWithNode func $ goF sdf
          DefAfterAllNode func sdf -> DefAfterAllNode func $ goF sdf
          DefParallelismNode p sdf -> DefParallelismNode p $ goF sdf

instance Foldable (SpecDefTree a c) where
  foldMap :: forall e m. Monoid m => (e -> m) -> SpecDefTree a c e -> m
  foldMap f =
    let goF :: forall x y. SpecDefForest x y e -> m
        goF = foldMap (foldMap f)
     in \case
          DefDescribeNode _ sdf -> goF sdf
          DefSpecifyNode _ _ e -> f e
          DefWrapNode _ sdf -> goF sdf
          DefAroundAllWithNode _ sdf -> goF sdf
          DefAfterAllNode _ sdf -> goF sdf
          DefParallelismNode _ sdf -> goF sdf

instance Traversable (SpecDefTree a c) where
  traverse :: forall u w f. Applicative f => (u -> f w) -> SpecDefTree a c u -> f (SpecDefTree a c w)
  traverse f =
    let goF :: forall x y. SpecDefForest x y u -> f (SpecDefForest x y w)
        goF = traverse (traverse f)
     in \case
          DefDescribeNode t sdf -> DefDescribeNode t <$> goF sdf
          DefSpecifyNode t td e -> DefSpecifyNode t td <$> f e
          DefWrapNode func sdf -> DefWrapNode func <$> goF sdf
          DefAroundAllWithNode func sdf -> DefAroundAllWithNode func <$> goF sdf
          DefAfterAllNode func sdf -> DefAfterAllNode func <$> goF sdf
          DefParallelismNode p sdf -> DefParallelismNode p <$> goF sdf

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
