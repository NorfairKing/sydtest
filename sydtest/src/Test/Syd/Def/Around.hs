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

module Test.Syd.Def.Around where

import Control.Monad.RWS.Strict
import Test.QuickCheck.IO ()
import Test.Syd.Def.TestDefM
import Test.Syd.HList
import Test.Syd.Run
import Test.Syd.SpecDef
import UnliftIO

-- | Run a custom action before every spec item.
before :: IO c -> TestDefM a c e -> TestDefM a () e
before action = around (action >>=)

-- | Run a custom action before every spec item.
before_ :: IO () -> TestDefM a c e -> TestDefM a c e
before_ action = around_ (action >>)

-- | Run a custom action after every spec item.
after :: (c -> IO ()) -> TestDefM a c e -> TestDefM a c e
after action = aroundWith $ \e x -> e x `finally` action x

-- | Run a custom action after every spec item.
after_ :: IO () -> TestDefM a c e -> TestDefM a c e
after_ action = after $ \_ -> action

-- | Run a custom action before and/or after every spec item.
around :: ((c -> IO ()) -> IO ()) -> TestDefM a c e -> TestDefM a () e
around action = aroundWith $ \e () -> action e

-- | Run a custom action before and/or after every spec item.
around_ :: (IO () -> IO ()) -> TestDefM a c e -> TestDefM a c e
around_ action = aroundWith $ \e a -> action (e a)

aroundWith :: forall a c d r. ((c -> IO ()) -> (d -> IO ())) -> TestDefM a c r -> TestDefM a d r
aroundWith func =
  aroundWith' $
    \( takeAC ::
         HList a -> c -> IO () -- Just to make sure the 'a' is not ambiguous.
       ) -- TODO try to get rid of this annotation
     a
     d ->
        func (\c -> takeAC a c) d

aroundWith' :: forall a c d r (u :: [*]). HContains (HList u) a => ((a -> c -> IO ()) -> (a -> d -> IO ())) -> TestDefM u c r -> TestDefM u d r
aroundWith' func (TestDefM rwst) = TestDefM $
  flip mapRWST rwst $ \inner -> do
    (res, s, forest) <- inner
    let modifyVal ::
          forall x.
          HContains (HList x) a =>
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
        modifyTree :: forall x e. HContains (HList x) a => SpecDefTree x c e -> SpecDefTree x d e
        modifyTree = \case
          DefDescribeNode t sdf -> DefDescribeNode t $ modifyForest sdf
          DefSpecifyNode t td e -> DefSpecifyNode t (modifyVal <$> td) e
          DefWrapNode f sdf -> DefWrapNode f $ modifyForest sdf
          DefBeforeAllNode f sdf -> DefBeforeAllNode f $ modifyForest sdf
          DefBeforeAllWithNode f sdf -> DefBeforeAllWithNode f $ modifyForest sdf
          DefAroundAllNode f sdf -> DefAroundAllNode f $ modifyForest sdf
          DefAroundAllWithNode f sdf -> DefAroundAllWithNode f $ modifyForest sdf
          DefAfterAllNode f sdf -> DefAfterAllNode f $ modifyForest sdf
          DefParallelismNode f sdf -> DefParallelismNode f $ modifyForest sdf
        modifyForest :: forall x e. HContains (HList x) a => SpecDefForest x c e -> SpecDefForest x d e
        modifyForest = map modifyTree
    let forest' :: SpecDefForest u d ()
        forest' = modifyForest forest
    pure (res, s, forest')
