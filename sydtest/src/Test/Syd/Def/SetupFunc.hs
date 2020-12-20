module Test.Syd.Def.SetupFunc where

import Test.Syd.Def.Around
import Test.Syd.Def.TestDefM

-- | A function that can provide an 'a' given a 'b'.
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

setupAroundWith :: SetupFunc () c -> TestDefM a c e -> TestDefM a () e
setupAroundWith (SetupFunc f) = aroundWith f

unwrapSetupFunc :: SetupFunc b a -> (b -> SetupFunc () a)
unwrapSetupFunc (SetupFunc provideAWithB) b = SetupFunc $ \takeA () ->
  provideAWithB (\a -> takeA a) b

wrapSetupFunc :: (b -> SetupFunc () a) -> SetupFunc b a
wrapSetupFunc bFunc = SetupFunc $ \takeA b ->
  let SetupFunc provideAWithUnit = bFunc b
   in provideAWithUnit (\a -> takeA a) ()

connectSetupFunc :: SetupFunc b a -> SetupFunc c b -> SetupFunc c a
connectSetupFunc (SetupFunc provideAWithB) (SetupFunc provideBWithC) = SetupFunc $ \takeA c ->
  provideBWithC
    ( \b ->
        provideAWithB
          ( \a -> takeA a
          )
          b
    )
    c
