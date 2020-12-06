{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines all the functions you will use to define your test suite.
module Test.Syd.Def.AroundAll where

import Control.Monad.RWS.Strict
import Test.QuickCheck.IO ()
import Test.Syd.Def.TestDefM
import Test.Syd.HList
import Test.Syd.SpecDef

-- | Run a custom action before all spec items.
beforeAll :: IO a -> TestDefM (a ': l) c e -> TestDefM l c e
beforeAll action = wrapRWST $ \forest -> DefBeforeAllNode action forest

-- | Run a custom action before all spec items.
beforeAll_ :: IO () -> TestDefM a b e -> TestDefM a b e
beforeAll_ action = aroundAll_ (action >>)

beforeAllWith :: (b -> IO a) -> TestDefM (a ': b ': l) c e -> TestDefM (b ': l) c e
beforeAllWith action = wrapRWST $ \forest -> DefBeforeAllWithNode action forest

-- | Run a custom action after all spec items.
afterAll :: (a -> IO ()) -> TestDefM (a ': l) b e -> TestDefM (a ': l) b e
afterAll func = afterAll' $ \(HCons a _) -> func a

afterAll' :: (HList l -> IO ()) -> TestDefM l b e -> TestDefM l b e
afterAll' func = wrapRWST $ \forest -> DefAfterAllNode func forest

-- | Run a custom action after all spec items.
afterAll_ :: IO () -> TestDefM a b e -> TestDefM a b e
afterAll_ action = afterAll' $ \_ -> action

-- | Run a custom action before and/or after all spec items.
aroundAll :: ((a -> IO ()) -> IO ()) -> TestDefM (a ': l) c e -> TestDefM l c e
aroundAll func = wrapRWST $ \forest -> DefAroundAllNode func forest

-- | Run a custom action before and/or after all spec items.
aroundAll_ :: (IO () -> IO ()) -> TestDefM a b e -> TestDefM a b e
aroundAll_ func = wrapRWST $ \forest -> DefWrapNode func forest

aroundAllWith ::
  forall a b c l r.
  ((a -> IO ()) -> (b -> IO ())) ->
  TestDefM (a ': b ': l) c r ->
  TestDefM (b ': l) c r
aroundAllWith func = wrapRWST $ \forest -> DefAroundAllWithNode func forest

wrapRWST :: (TestForest a c -> TestTree b d) -> TestDefM a c l -> TestDefM b d l
wrapRWST func (TestDefM rwst) = TestDefM $
  flip mapRWST rwst $ \inner -> do
    (res, s, forest) <- inner
    let forest' = [func forest]
    pure (res, s, forest')
