{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines all the functions you will use to define your test suite.
module Test.Syd.Def.AroundAll where

import Control.Monad.Writer.Strict
import Test.QuickCheck.IO ()
import Test.Syd.Def.TestDefM
import Test.Syd.HList
import Test.Syd.SpecDef

-- | Run a custom action before all spec items in a group, to set up an outer resource 'a'.
beforeAll ::
  -- | The function to run (once), beforehand, to produce the outer resource.
  IO outer ->
  TestDefM (outer ': otherOuters) inner result ->
  TestDefM otherOuters inner result
beforeAll action = wrapForest $ \forest -> DefBeforeAllNode action forest

-- | Run a custom action before all spec items in a group without setting up any outer resources.
beforeAll_ ::
  -- | The function to run (once), beforehand.
  IO () ->
  TestDefM outers inner result ->
  TestDefM outers inner result
beforeAll_ action = aroundAll_ (action >>)

-- | Run a custom action before all spec items in a group, to set up an outer resource 'b' by using the outer resource 'a'.
beforeAllWith ::
  -- | The function to run (once), beforehand, to produce a new outer resource while using a previous outer resource
  (previousOuter -> IO newOuter) ->
  TestDefM (newOuter ': previousOuter ': otherOuters) inner result ->
  TestDefM (previousOuter ': otherOuters) inner result
beforeAllWith action = aroundAllWith $ \func b -> do
  a <- action b
  func a

-- | Run a custom action after all spec items, using the outer resource 'a'.
afterAll ::
  -- | The function to run (once), afterwards, using the outer resource.
  (outer -> IO ()) ->
  TestDefM (outer ': otherOuters) inner result ->
  TestDefM (outer ': otherOuters) inner result
afterAll func = afterAll' $ \(HCons a _) -> func a

-- | Run a custom action after all spec items, using all the outer resources.
afterAll' ::
  -- | The function to run (once), afterwards, using all outer resources.
  (HList outers -> IO ()) ->
  TestDefM outers inner result ->
  TestDefM outers inner result
afterAll' func = wrapForest $ \forest -> DefAfterAllNode func forest

-- | Run a custom action after all spec items without using any outer resources.
afterAll_ ::
  -- | The function to run (once), afterwards.
  IO () ->
  TestDefM outers inner result ->
  TestDefM outers inner result
afterAll_ action = afterAll' $ \_ -> action

-- | Run a custom action before and/or after all spec items in group, to provide access to a resource 'a'.
--
-- See the @FOOTGUN@ note in the docs for 'around_'.
aroundAll ::
  -- | The function that provides the outer resource (once), around the tests.
  ((outer -> IO ()) -> IO ()) ->
  TestDefM (outer ': otherOuters) inner result ->
  TestDefM otherOuters inner result
aroundAll func = wrapForest $ \forest -> DefAroundAllNode func forest

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
aroundAll_ ::
  -- | The function that wraps running the tests.
  (IO () -> IO ()) ->
  TestDefM outers inner result ->
  TestDefM outers inner result
aroundAll_ func = wrapForest $ \forest -> DefWrapNode func forest

-- | Run a custom action before and/or after all spec items in a group to provide access to a resource 'a' while using a resource 'b'
--
-- See the @FOOTGUN@ note in the docs for 'around_'.
aroundAllWith ::
  forall newOuter oldOuter otherOuters inner result.
  -- | The function that provides the new outer resource (once), using the old outer resource.
  ((newOuter -> IO ()) -> (oldOuter -> IO ())) ->
  TestDefM (newOuter ': oldOuter ': otherOuters) inner result ->
  TestDefM (oldOuter ': otherOuters) inner result
aroundAllWith func = wrapForest $ \forest -> DefAroundAllWithNode func forest

-- | Declare a node in the spec def forest
wrapForest ::
  -- | The wrapper node
  (TestForest outers1 inner1 -> TestTree outers2 inner2) ->
  TestDefM outers1 inner1 result ->
  TestDefM outers2 inner2 result
wrapForest func (TestDefM rwst) = TestDefM $
  flip mapWriterT rwst $ \inner -> do
    (res, forest) <- inner
    let forest' = [func forest]
    pure (res, forest')
