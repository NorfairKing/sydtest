module Example.ExceptionLibSpec (spec) where

import Example.ExceptionLib
import Example.Gen
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec =
  describe "addOneModuleDisabled" $
    it "increments by one" $
      forAll genInt $ \n ->
        addOneModuleDisabled n `shouldBe` n + 1
