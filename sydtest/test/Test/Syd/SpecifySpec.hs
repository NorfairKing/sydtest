{-# LANGUAGE GADTs #-}

module Test.Syd.SpecifySpec (spec) where

import Test.Syd

spec :: Spec
spec = sequential $ do
  describe "boolean" $ do
    it "boolean" True
    before (pure (2 :: Int)) $ it "boolean function (inner)" $ \i -> even i
    beforeAll (pure (2 :: Int)) $ itWithOuter "boolean function (outer)" $ \i () -> even i
