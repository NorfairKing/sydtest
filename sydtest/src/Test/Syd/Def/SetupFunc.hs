{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | The 'SetupFunc' abstraction makes resource provider functions (of type @(a -> IO r) -> IO r@) composable.
module Test.Syd.Def.SetupFunc where

import Control.Category as Cat
import Control.Monad.IO.Class
import Test.Syd.Def.Around
import Test.Syd.Def.AroundAll
import Test.Syd.Def.TestDefM
import Test.Syd.HList

-- | A function that can provide an 'new' given an 'old'.
--
-- You can think of this as a potentially-resource-aware version of 'old -> IO new'.
--
-- This type has a monad instance, which means you can now compose setup functions using regular do-notation.
newtype SetupFunc old new = SetupFunc
  { unSetupFunc :: forall r. (new -> IO r) -> (old -> IO r)
  }

instance Functor (SetupFunc old) where
  fmap f (SetupFunc provideA) = SetupFunc $ \takeB c ->
    let takeA = \a -> takeB $ f a
     in provideA takeA c

instance Applicative (SetupFunc old) where
  pure a = SetupFunc $ \aFunc _ -> aFunc a
  (SetupFunc provideF) <*> (SetupFunc provideA) = SetupFunc $ \takeB c ->
    provideF
      ( \f ->
          provideA
            ( \a ->
                takeB (f a)
            )
            c
      )
      c

instance Monad (SetupFunc old) where
  (SetupFunc provideA) >>= m = SetupFunc $ \takeB c ->
    provideA
      ( \a ->
          let (SetupFunc provideB) = m a
           in provideB (\b -> takeB b) c
      )
      c

instance MonadIO (SetupFunc old) where
  liftIO ioFunc = SetupFunc $ \takeA _ -> do
    ioFunc >>= takeA

instance Category SetupFunc where
  id = SetupFunc Prelude.id
  (.) = composeSetupFunc

-- | Turn a simple provider function into a 'SetupFunc'.
--
-- This works together nicely with most supplier functions.
-- Some examples:
--
-- * [Network.Wai.Handler.Warp.testWithApplication](https://hackage.haskell.org/package/warp-3.3.13/docs/Network-Wai-Handler-Warp.html#v:testWithApplication)
-- * [Path.IO.withSystemTempDir](https://hackage.haskell.org/package/path-io-1.6.2/docs/Path-IO.html#v:withSystemTempDir)
makeSimpleSetupFunc ::
  (forall result. (resource -> IO result) -> IO result) ->
  SetupFunc () resource
makeSimpleSetupFunc provideA = SetupFunc $ \takeA () -> provideA $ \a -> takeA a

-- | Use a 'SetupFunc ()' as a simple provider function.
--
-- This is the opposite of the 'makeSimpleSetupFunc' function
useSimpleSetupFunc ::
  SetupFunc () resource -> (forall result. (resource -> IO result) -> IO result)
useSimpleSetupFunc (SetupFunc provideAWithUnit) takeA = provideAWithUnit (\a -> takeA a) ()

-- | Wrap a function that produces a 'SetupFunc' to into a 'SetupFunc'.
--
-- This is useful to combine a given 'SetupFunc b' with other 'SetupFunc ()'s as follows:
--
-- > mySetupFunc :: SetupFunc B A
-- > mySetupFunc = wrapSetupFunc $ \b -> do
-- >   r <- setupSomething
-- >   c <- setupSomethingElse b r
-- >   pure $ somehowCombine c r
-- >
-- > setupSomething :: SetupFunc () R
-- > setupSomething :: B -> R -> SetupFunc () C
-- > somehowCombine :: C -> R -> A
wrapSetupFunc ::
  (old -> SetupFunc () new) ->
  SetupFunc old new
wrapSetupFunc bFunc = SetupFunc $ \takeA b ->
  let SetupFunc provideAWithUnit = bFunc b
   in provideAWithUnit (\a -> takeA a) ()

-- | Unwrap a 'SetupFunc' into a function that produces a 'SetupFunc'
--
-- This is the opposite of 'wrapSetupFunc'.
unwrapSetupFunc ::
  SetupFunc old new -> (old -> SetupFunc () new)
unwrapSetupFunc (SetupFunc provideAWithB) b = SetupFunc $ \takeA () ->
  provideAWithB (\a -> takeA a) b

-- | Compose two setup functions.
--
-- This is '(.)' but for 'SetupFunc's
composeSetupFunc ::
  SetupFunc newer newest ->
  SetupFunc old newer ->
  SetupFunc old newest
composeSetupFunc (SetupFunc provideAWithB) (SetupFunc provideBWithC) = SetupFunc $ \takeA c ->
  provideBWithC
    ( \b ->
        provideAWithB
          ( \a -> takeA a
          )
          b
    )
    c

-- | Connect two setup functions.
--
-- This is basically 'flip (.)' but for 'SetupFunc's.
-- It's exactly 'flip composeSetupFunc'.
connectSetupFunc ::
  SetupFunc old newer ->
  SetupFunc newer newest ->
  SetupFunc old newest
connectSetupFunc = flip composeSetupFunc

-- | Use 'around' with a 'SetupFunc'
setupAround ::
  SetupFunc () inner ->
  TestDefM outers inner result ->
  TestDefM outers () result
setupAround = setupAroundWith

-- | Use 'aroundWith' with a 'SetupFunc'
setupAroundWith ::
  SetupFunc oldInner newInner ->
  TestDefM outers newInner result ->
  TestDefM outers oldInner result
setupAroundWith (SetupFunc f) = aroundWith f

-- | Use 'aroundWith'' with a 'SetupFunc'
setupAroundWith' ::
  HContains outers outer =>
  (outer -> SetupFunc oldInner newInner) ->
  TestDefM outers newInner result ->
  TestDefM outers oldInner result
setupAroundWith' setupFuncFunc = aroundWith' $ \takeAC a d ->
  let (SetupFunc provideCWithD) = setupFuncFunc a
   in provideCWithD (\c -> takeAC a c) d

-- | Use 'aroundAll' with a 'SetupFunc'
setupAroundAll ::
  SetupFunc () outer ->
  TestDefM (outer : outers) inner result ->
  TestDefM outers inner result
setupAroundAll sf = aroundAll $ \func -> unSetupFunc sf func ()

-- | Use 'aroundAllWith' with a 'SetupFunc'
setupAroundAllWith ::
  SetupFunc oldOuter newOuter ->
  TestDefM (newOuter ': oldOuter ': outers) inner result ->
  TestDefM (oldOuter ': outers) inner result
setupAroundAllWith sf = aroundAllWith $ unSetupFunc sf
