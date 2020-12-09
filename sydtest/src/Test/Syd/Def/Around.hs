{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Def.Around where

import Control.Monad.RWS.Strict
import Test.QuickCheck.IO ()
import Test.Syd.Def.TestDefM
import Test.Syd.HList
import Test.Syd.Run
import Test.Syd.SpecDef
import UnliftIO

-- | Run a custom action before every spec item, to set up an inner resource 'c'.
before :: IO c -> TestDefM a c e -> TestDefM a () e
before action = around (action >>=)

-- | Run a custom action before every spec item without setting up any inner resources.
before_ :: IO () -> TestDefM a c e -> TestDefM a c e
before_ action = around_ (action >>)

-- | Run a custom action after every spec item, using the inner resource 'c'.
after :: (c -> IO ()) -> TestDefM a c e -> TestDefM a c e
after action = aroundWith $ \e x -> e x `finally` action x

-- | Run a custom action after every spec item without using any inner resources.
after_ :: IO () -> TestDefM a c e -> TestDefM a c e
after_ action = after $ \_ -> action

-- | Run a custom action before and/or after every spec item, to provide access to an inner resource 'c'.
--
-- See the @FOOTGUN@ note in the docs for 'around_'.
around :: ((c -> IO ()) -> IO ()) -> TestDefM a c e -> TestDefM a () e
around action = aroundWith $ \e () -> action e

-- | Run a custom action before and/or after every spec item without accessing any inner resources.
--
-- It is important that the wrapper function that you provide runs the action that it gets _exactly once_.
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
-- >    around_ don'tDo $ do
-- >      it "should pass" True
--
-- During execution, you'll then get an error like this:
--
-- > thread blocked indefinitely in an MVar operation
--
-- The same problem exists when using 'Test.Syd.Def.Around.aroundAll_'.
--
-- The same thing will go wrong if you run the given action more than once like this:
--
-- > spec :: Spec
-- > spec = do
-- >    let doTwice :: IO () -> IO ()
-- >        doTwice f = f >> f
-- >    around_ doTwice $ do
-- >      it "should pass" True
--
--
-- Note: If you're interested in fixing this, talk to me, but only after GHC has gotten impredicative types because that will likely be a requirement.
around_ :: (IO () -> IO ()) -> TestDefM a c e -> TestDefM a c e
around_ action = aroundWith $ \e a -> action (e a)

-- | Run a custom action before and/or after every spec item, to provide access to an inner resource 'c' while using the inner resource 'd'.
--
-- See the @FOOTGUN@ note in the docs for 'around_'.
aroundWith :: forall a c d r. ((c -> IO ()) -> (d -> IO ())) -> TestDefM a c r -> TestDefM a d r
aroundWith func =
  aroundWith' $
    \(takeAC :: HList a -> c -> IO ()) -- Just to make sure the 'a' is not ambiguous.
     a
     d ->
        func (\c -> takeAC a c) d

-- | Run a custom action before and/or after every spec item, to provide access to an inner resource 'c' while using the inner resource 'd' and any outer resource available.
aroundWith' :: forall a c d r (u :: [*]). HContains u a => ((a -> c -> IO ()) -> (a -> d -> IO ())) -> TestDefM u c r -> TestDefM u d r
aroundWith' func (TestDefM rwst) = TestDefM $
  flip mapRWST rwst $ \inner -> do
    (res, s, forest) <- inner
    let modifyVal ::
          forall x.
          HContains x a =>
          (((HList x -> c -> IO ()) -> IO ()) -> IO TestRunResult) ->
          ((HList x -> d -> IO ()) -> IO ()) ->
          IO TestRunResult
        modifyVal takeSupplyXC supplyXD =
          let supplyXC :: (HList x -> c -> IO ()) -> IO ()
              supplyXC takeXC =
                let takeXD :: HList x -> d -> IO ()
                    takeXD x d =
                      let takeAC _ c = takeXC x c
                       in func takeAC (getElem x) d
                 in supplyXD takeXD
           in takeSupplyXC supplyXC

        -- For this function to work recursively, the first parameter of the input and the output types must be the same
        modifyTree :: forall x e. HContains x a => SpecDefTree x c e -> SpecDefTree x d e
        modifyTree = \case
          DefDescribeNode t sdf -> DefDescribeNode t $ modifyForest sdf
          DefPendingNode t mr -> DefPendingNode t mr
          DefSpecifyNode t td e -> DefSpecifyNode t (modifyVal <$> td) e
          DefWrapNode f sdf -> DefWrapNode f $ modifyForest sdf
          DefBeforeAllNode f sdf -> DefBeforeAllNode f $ modifyForest sdf
          DefAroundAllNode f sdf -> DefAroundAllNode f $ modifyForest sdf
          DefAroundAllWithNode f sdf -> DefAroundAllWithNode f $ modifyForest sdf
          DefAfterAllNode f sdf -> DefAfterAllNode f $ modifyForest sdf
          DefParallelismNode f sdf -> DefParallelismNode f $ modifyForest sdf
          DefRandomisationNode f sdf -> DefRandomisationNode f $ modifyForest sdf
        modifyForest :: forall x e. HContains x a => SpecDefForest x c e -> SpecDefForest x d e
        modifyForest = map modifyTree
    let forest' :: SpecDefForest u d ()
        forest' = modifyForest forest
    pure (res, s, forest')
