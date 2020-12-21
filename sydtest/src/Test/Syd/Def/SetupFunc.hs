module Test.Syd.Def.SetupFunc where

import Control.Monad.IO.Class
import Test.Syd.Def.Around
import Test.Syd.Def.TestDefM
import Test.Syd.HList

-- | A function that can provide an 'a' given a 'b'.
--
-- You can think of this as a potentially-resource-aware version of 'b -> IO a'.
newtype SetupFunc b a = SetupFunc
  { unSetupFunc :: (a -> IO ()) -> (b -> IO ())
  }

instance Functor (SetupFunc c) where
  fmap f (SetupFunc provideA) = SetupFunc $ \takeB c ->
    let takeA = \a -> takeB $ f a
     in provideA takeA c

instance Applicative (SetupFunc c) where
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

instance Monad (SetupFunc c) where
  (SetupFunc provideA) >>= m = SetupFunc $ \takeB c ->
    provideA
      ( \a ->
          let (SetupFunc provideB) = m a
           in provideB (\b -> takeB b) c
      )
      c

instance MonadIO (SetupFunc c) where
  liftIO ioFunc = SetupFunc $ \takeA _ -> do
    ioFunc >>= takeA

-- | Turn a simple provider function into a 'SetupFunc'.
makeSimpleSetupFunc :: ((a -> IO ()) -> IO ()) -> SetupFunc () a
makeSimpleSetupFunc provideA = SetupFunc $ \takeA () -> provideA $ \a -> takeA a

-- | Use a 'SetupFunc ()' as a simple provider function.
useSimpleSetupFunc :: SetupFunc () a -> ((a -> IO ()) -> IO ())
useSimpleSetupFunc (SetupFunc provideAWithUnit) takeA = provideAWithUnit (\a -> takeA a) ()

-- | Unwrap a 'SetupFunc' into a function that produces a 'SetupFunc'
unwrapSetupFunc :: SetupFunc b a -> (b -> SetupFunc () a)
unwrapSetupFunc (SetupFunc provideAWithB) b = SetupFunc $ \takeA () ->
  provideAWithB (\a -> takeA a) b

-- | Wrap a function that produces a 'SetupFunc' to into a 'SetupFunc'.
wrapSetupFunc :: (b -> SetupFunc () a) -> SetupFunc b a
wrapSetupFunc bFunc = SetupFunc $ \takeA b ->
  let SetupFunc provideAWithUnit = bFunc b
   in provideAWithUnit (\a -> takeA a) ()

-- | Compose two setup functions.
--
-- This is basically '(.)' but for 'SetupFunc's
composeSetupFunc :: SetupFunc b a -> SetupFunc c b -> SetupFunc c a
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
connectSetupFunc :: SetupFunc c b -> SetupFunc b a -> SetupFunc c a
connectSetupFunc = flip composeSetupFunc

-- | Use 'around' with a 'SetupFunc'
setupAround :: SetupFunc () c -> TestDefM a c e -> TestDefM a () e
setupAround = setupAroundWith

-- | Use 'aroundWith' with a 'SetupFunc'
setupAroundWith :: SetupFunc d c -> TestDefM a c e -> TestDefM a d e
setupAroundWith (SetupFunc f) = aroundWith f

-- | Use 'aroundWith'' with a 'SetupFunc'
setupAroundWith' :: HContains l a => (a -> SetupFunc d c) -> TestDefM l c e -> TestDefM l d e
setupAroundWith' setupFuncFunc = aroundWith' $ \takeAC a d ->
  let (SetupFunc provideCWithD) = setupFuncFunc a
   in provideCWithD (\c -> takeAC a c) d
