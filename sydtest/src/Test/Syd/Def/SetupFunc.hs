module Test.Syd.Def.SetupFunc where

newtype SetupFunc a = SetupFunc
  { unSetupFunc :: (a -> IO ()) -> IO ()
  }

instance Functor SetupFunc where
  fmap f (SetupFunc provideA) = SetupFunc $ \takeB ->
    let takeA = \a -> takeB $ f a
     in provideA takeA

instance Applicative SetupFunc where
  pure a = SetupFunc $ \aFunc -> aFunc a
  (SetupFunc provideF) <*> (SetupFunc provideA) = SetupFunc $ \takeB ->
    provideF $ \f ->
      provideA $ \a ->
        takeB (f a)

instance Monad SetupFunc where
  (SetupFunc provideA) >>= m = SetupFunc $ \takeB ->
    provideA $ \a ->
      let (SetupFunc provideB) = m a
       in provideB $ \b -> takeB b
