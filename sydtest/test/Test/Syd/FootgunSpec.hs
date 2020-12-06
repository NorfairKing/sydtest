{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.FootgunSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  aroundWithTwice
  aroundWithDon't

aroundWithDon't :: Spec
aroundWithDon't = do
  -- -- This causes sydtest-test: thread blocked indefinitely in an MVar operation
  -- let don'tDo :: IO () -> IO ()
  --     don'tDo _ = pure ()
  -- around_ don'tDo $ do
  --   it "should pass" True
  pure ()

aroundWithTwice :: Spec
aroundWithTwice = do
  -- -- This causes sydtest-test: thread blocked indefinitely in an MVar operation
  -- let doTwice :: IO () -> IO ()
  --     doTwice f = f >> f
  -- around_ doTwice $ do
  --   it "should pass" True
  pure ()
