{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Specify.AllOuterSpec (spec) where

import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = sequential $ do
  describe "boolean" $ do
    beforeAll (pure (1 :: Int)) $
      beforeAll (pure (2 :: Int)) $
        itWithAllOuter "boolean function (all outer)" $
          \(HCons i (HCons j HNil) :: HList '[Int, Int]) () -> even i && odd j

  describe "IO action" $ do
    beforeAll (pure (1 :: Int)) $ do
      beforeAll (pure (2 :: Int)) $
        itWithAllOuter "IO action function (all outer)" $
          \(HCons i (HCons j HNil) :: HList '[Int, Int]) () -> (i, j) `shouldBe` (2, 1)

  describe "property test" $ do
    beforeAll (pure (1 :: Int)) $ do
      beforeAll (pure (2 :: Int)) $
        itWithAllOuter "property test function (all outer)" $
          \(HCons i (HCons j HNil) :: HList '[Int, Int]) () ->
            property $ \k -> i * j * k `shouldBe` 1 * 2 * k
