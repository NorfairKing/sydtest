{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.SpecifySpec (spec) where

import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = sequential $ do
  describe "boolean" $ do
    it "boolean" True
    before (pure (2 :: Int)) $
      it "boolean function (inner)" $ \i -> even i
    beforeAll (pure (2 :: Int)) $
      itWithOuter "boolean function (one outer)" $ \i () -> even i
    beforeAll (pure (1 :: Int)) $
      beforeAll (pure (2 :: Int)) $
        itWithAllOuter "boolean function (all outer)" $
          \(HCons i (HCons j HNil) :: HList '[Int, Int]) () -> even i && odd j

  describe "IO action" $ do
    it "IO action" True
    before (pure (2 :: Int)) $
      it "IO action function (inner)" $ \i -> i `shouldBe` 2
    beforeAll (pure (1 :: Int)) $ do
      itWithOuter "IO action function (one outer)" $ \i () -> i `shouldBe` 1
      beforeAll (pure (2 :: Int)) $
        itWithAllOuter "IO action function (all outer)" $
          \(HCons i (HCons j HNil) :: HList '[Int, Int]) () -> (i, j) `shouldBe` (2, 1)

  describe "property test" $ do
    it "property test" $ property $ \j -> j `shouldBe` (j :: Int)
    before (pure (2 :: Int)) $
      it "property test function (inner)" $ \i ->
        property $ \j ->
          i * j `shouldBe` 2 * (j :: Int)
    beforeAll (pure (1 :: Int)) $ do
      itWithOuter "property test function (one outer)" $ \i () -> property $ \j ->
        i * j `shouldBe` 1 * j
      beforeAll (pure (2 :: Int)) $
        itWithAllOuter "property test function (all outer)" $
          \(HCons i (HCons j HNil) :: HList '[Int, Int]) () ->
            property $ \k -> i * j * k `shouldBe` 1 * 2 * k
