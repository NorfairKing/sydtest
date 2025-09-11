{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | The 'SetupFunc' abstraction makes resource provider functions (of type @(a -> IO r) -> IO r@) composable.
module Test.Syd.Def.SetupFunc where

import Control.Exception
import Control.Monad.IO.Class
import Test.Syd.Def.Around
import Test.Syd.Def.AroundAll
import Test.Syd.Def.TestDefM
import Test.Syd.HList

-- * Creating 'SetupFunc's

-- | A function that can provide a 'resource'.
--
-- You can think of this as a potentially-resource-aware version of 'IO resource'.
-- In other words, it's like an 'IO resource' that can clean up after itself.
--
-- This type has a monad instance, which means you can now compose setup functions using regular do-notation.
-- This works together nicely with most supplier functions.
-- Some examples:
--
-- * [Network.Wai.Handler.Warp.testWithApplication](https://hackage.haskell.org/package/warp-3.3.13/docs/Network-Wai-Handler-Warp.html#v:testWithApplication)
-- * [Path.IO.withSystemTempDir](https://hackage.haskell.org/package/path-io-1.6.2/docs/Path-IO.html#v:withSystemTempDir)
--
-- Note that these examples already have functions defined for them in sydtest companion libraries.
newtype SetupFunc resource = SetupFunc
  { unSetupFunc :: forall r. (resource -> IO r) -> IO r
  }

instance Functor SetupFunc where
  fmap f (SetupFunc provideA) = SetupFunc $ \takeB ->
    let takeA = \a -> takeB $ f a
     in provideA takeA

instance Applicative SetupFunc where
  pure a = SetupFunc $ \aFunc -> aFunc a
  (SetupFunc provideF) <*> (SetupFunc provideA) = SetupFunc $ \takeB ->
    provideF
      ( \f ->
          provideA
            ( \a ->
                takeB (f a)
            )
      )

instance Monad SetupFunc where
  (SetupFunc provideA) >>= m = SetupFunc $ \takeB ->
    provideA
      ( \a ->
          let (SetupFunc provideB) = m a
           in provideB (\b -> takeB b)
      )

instance MonadIO SetupFunc where
  liftIO ioFunc = SetupFunc $ \takeA -> do
    ioFunc >>= takeA

-- | Turn the arguments that you would normally give to 'bracket' into a 'SetupFunc'.
bracketSetupFunc :: IO resource -> (resource -> IO r) -> SetupFunc resource
bracketSetupFunc acquire release = SetupFunc $ \func -> bracket acquire release func

-- * Using 'SetupFunc' to define your test suite

-- | Use 'around' with a 'SetupFunc'
setupAround ::
  SetupFunc inner ->
  TestDefM outers inner result ->
  TestDefM outers any result
setupAround setupFunc = setupAroundWith $ \_ -> setupFunc

-- | Use 'aroundWith' with a 'SetupFunc'
setupAroundWith ::
  (oldInner -> SetupFunc newInner) ->
  TestDefM outers newInner result ->
  TestDefM outers oldInner result
setupAroundWith takeOldInner = aroundWith $ \takeNewInner oldInner ->
  let SetupFunc provideNewInner = takeOldInner oldInner
   in provideNewInner $ \newInner -> takeNewInner newInner

-- | Use 'aroundWith'' with a 'SetupFunc'
setupAroundWith' ::
  (HContains outers outer) =>
  (outer -> oldInner -> SetupFunc newInner) ->
  TestDefM outers newInner result ->
  TestDefM outers oldInner result
setupAroundWith' f = aroundWith' $ \takeBoth outer oldInner ->
  let SetupFunc provideNewInner = f outer oldInner
   in provideNewInner $ \newInner -> takeBoth outer newInner

-- | Use all outer resources and the inner resource to provide a new inner resource
-- This is a more constrained version of 'setupAroundWith'' to more easily allow providing an inner resource based on multiple outer resources
setupAroundWithAll :: (HList outers -> oldInner -> SetupFunc newInner) -> TestDefM outers newInner result -> TestDefM outers oldInner result
setupAroundWithAll = setupAroundWith'

-- | Use 'aroundAll' with a 'SetupFunc'
setupAroundAll ::
  SetupFunc outer ->
  TestDefM (outer : outers) inner result ->
  TestDefM outers inner result
setupAroundAll sf = aroundAll $ \func -> unSetupFunc sf func

-- | Use 'aroundAllWith' with a 'SetupFunc'
setupAroundAllWith ::
  (oldOuter -> SetupFunc newOuter) ->
  TestDefM (newOuter ': oldOuter ': outers) inner result ->
  TestDefM (oldOuter ': outers) inner result
setupAroundAllWith sf = aroundAllWith $ \takeNewOuter oldOuter ->
  let SetupFunc provideNewOuter = sf oldOuter
   in provideNewOuter $ \newOuter -> takeNewOuter newOuter

-- | Use 'aroundAllWithAll' with a 'SetupFunc'
setupAroundAllWithAll ::
  (HList (oldOuter ': outers) -> SetupFunc newOuter) ->
  TestDefM (newOuter ': oldOuter ': outers) inner result ->
  TestDefM (oldOuter ': outers) inner result
setupAroundAllWithAll sf = aroundAllWithAll $ \takeNewOuter oldOuter ->
  let SetupFunc provideNewOuter = sf oldOuter
   in provideNewOuter $ \newOuter -> takeNewOuter newOuter
