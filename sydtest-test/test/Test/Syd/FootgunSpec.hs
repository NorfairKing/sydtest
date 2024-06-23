{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.FootgunSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  aroundTwice
  aroundDon't
  aroundAllTwice
  aroundAllDon't

aroundDon't :: Spec
aroundDon't = do
  -- -- This causes sydtest-test: thread blocked indefinitely in an MVar operation
  -- let don'tDo :: IO () -> IO ()
  --     don'tDo _ = pure ()
  -- around_ don'tDo $ do
  --   it "should pass" True
  pure ()

aroundTwice :: Spec
aroundTwice = do
  -- -- This causes sydtest-test: thread blocked indefinitely in an MVar operation
  -- let doTwice :: IO () -> IO ()
  --     doTwice f = f >> f
  -- around_ doTwice $ do
  --   it "should pass" True
  pure ()

aroundAllDon't :: Spec
aroundAllDon't = do
  -- -- This causes sydtest-test: thread blocked indefinitely in an MVar operation
  -- let don'tDo :: IO () -> IO ()
  --     don'tDo _ = pure ()
  -- aroundAll_ don'tDo $ do
  --   it "should pass" True
  pure ()

aroundAllTwice :: Spec
aroundAllTwice = do
  -- -- This 'just works' but takes longer.
  -- let doTwice :: IO () -> IO ()
  --     doTwice f = f >> f
  -- aroundAll_ doTwice $ do
  --   it "should pass" True
  pure ()
