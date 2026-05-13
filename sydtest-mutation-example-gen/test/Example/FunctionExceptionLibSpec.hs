module Example.FunctionExceptionLibSpec (spec) where

import Example.FunctionExceptionLib
import Example.Gen
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = do
  describe "addOneArithDisabled" $
    it "increments by one" $
      forAll genInt $ \n ->
        addOneArithDisabled n `shouldBe` n + 1
  describe "addOneArithAndIntLitDisabled" $
    it "increments by one" $
      forAll genInt $ \n ->
        addOneArithAndIntLitDisabled n `shouldBe` n + 1
  describe "addOneFunctionDisabled" $
    it "increments by one" $
      forAll genInt $ \n ->
        addOneFunctionDisabled n `shouldBe` n + 1
