{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines all the functions you will use to define your test suite.
module Test.Syd.Def.AroundAll where

import Control.Monad.RWS.Strict
import Test.QuickCheck.IO ()
import Test.Syd.Def.SetupFunc
import Test.Syd.Def.TestDefM
import Test.Syd.HList
import Test.Syd.SpecDef

-- | Run a custom action before all spec items in a group, to set up an outer resource 'a'.
beforeAll :: IO a -> TestDefM (a ': l) c e -> TestDefM l c e
beforeAll action = wrapRWST $ \forest -> DefBeforeAllNode action forest

-- | Run a custom action before all spec items in a group without setting up any outer resources.
beforeAll_ :: IO () -> TestDefM a b e -> TestDefM a b e
beforeAll_ action = aroundAll_ (action >>)

-- | Run a custom action before all spec items in a group, to set up an outer resource 'b' by using the outer resource 'a'.
beforeAllWith :: (b -> IO a) -> TestDefM (a ': b ': l) c e -> TestDefM (b ': l) c e
beforeAllWith action = aroundAllWith $ \func b -> do
  a <- action b
  func a

-- | Run a custom action after all spec items, using the outer resource 'a'.
afterAll :: (a -> IO ()) -> TestDefM (a ': l) b e -> TestDefM (a ': l) b e
afterAll func = afterAll' $ \(HCons a _) -> func a

-- | Run a custom action after all spec items, using all the outer resources.
afterAll' :: (HList l -> IO ()) -> TestDefM l b e -> TestDefM l b e
afterAll' func = wrapRWST $ \forest -> DefAfterAllNode func forest

-- | Run a custom action after all spec items without using any outer resources.
afterAll_ :: IO () -> TestDefM a b e -> TestDefM a b e
afterAll_ action = afterAll' $ \_ -> action

-- | Run a custom action before and/or after all spec items in group, to provide access to a resource 'a'.
--
-- See the @FOOTGUN@ note in the docs for 'around_'.
aroundAll :: ((a -> IO ()) -> IO ()) -> TestDefM (a ': l) c e -> TestDefM l c e
aroundAll func = wrapRWST $ \forest -> DefAroundAllNode func forest

-- | Run a custom action before and/or after all spec items in a group without accessing any resources.
--
-- == __FOOTGUN__
--
-- This combinator gives the programmer a lot of power.
-- In fact, it gives the programmer enough power to break the test framework.
-- Indeed, you can provide a wrapper function that just _doesn't_ run the function like this:
--
-- > spec :: Spec
-- > spec = do
-- >    let don'tDo :: IO () -> IO ()
-- >        don'tDo _ = pure ()
-- >    aroundAll_ don'tDo $ do
-- >      it "should pass" True
--
-- During execution, you'll then get an error like this:
--
-- > thread blocked indefinitely in an MVar operation
--
-- The same problem exists when using 'Test.Syd.Def.Around.around_'.
--
-- Something even more pernicious goes wrong when you run the given action more than once like this:
--
-- > spec :: Spec
-- > spec = do
-- >    let doTwice :: IO () -> IO ()
-- >        doTwice f = f >> f
-- >    aroundAll_ doTwice $ do
-- >      it "should pass" True
--
-- In this case, the test will "just work", but it will be executed twice even if the output reports that it only passed once.
--
-- Note: If you're interested in fixing this, talk to me, but only after GHC has gotten impredicative types because that will likely be a requirement.
aroundAll_ :: (IO () -> IO ()) -> TestDefM a b e -> TestDefM a b e
aroundAll_ func = wrapRWST $ \forest -> DefWrapNode func forest

-- | Run a custom action before and/or after all spec items in a group to provide access to a resource 'a' while using a resource 'b'
--
-- See the @FOOTGUN@ note in the docs for 'around_'.
aroundAllWith ::
  forall a b c l r.
  (forall s. (a -> IO s) -> (b -> IO s)) ->
  TestDefM (a ': b ': l) c r ->
  TestDefM (b ': l) c r
aroundAllWith func = wrapRWST $ \forest -> DefAroundAllWithNode (SetupFunc func) forest

-- | Declare a node in the spec def forest
wrapRWST :: (TestForest a c -> TestTree b d) -> TestDefM a c l -> TestDefM b d l
wrapRWST func (TestDefM rwst) = TestDefM $
  flip mapRWST rwst $ \inner -> do
    (res, s, forest) <- inner
    let forest' = [func forest]
    pure (res, s, forest')
