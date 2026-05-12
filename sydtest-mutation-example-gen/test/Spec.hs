module Main (main) where

import Example.ExceptionLib (addOneModuleDisabled)
import Example.FunctionExceptionLib
  ( addOneArithAndIntLitDisabled,
    addOneArithDisabled,
    addOneFunctionDisabled,
  )
import Example.Gen (genInt)
import Example.Lib (addOne)
import Test.QuickCheck (forAll)
import Test.Syd (describe, it, shouldBe, sydTest)

main :: IO ()
main = sydTest $ do
  describe "addOne" $ do
    it "increments by one" $
      forAll genInt $ \n ->
        addOne n `shouldBe` n + 1
    it "is greater than its input" $
      forAll genInt $ \n ->
        addOne n > n
  describe "addOneModuleDisabled" $ do
    it "increments by one" $
      forAll genInt $ \n ->
        addOneModuleDisabled n `shouldBe` n + 1
  describe "addOneArithDisabled" $ do
    it "increments by one" $
      forAll genInt $ \n ->
        addOneArithDisabled n `shouldBe` n + 1
  describe "addOneArithAndIntLitDisabled" $ do
    it "increments by one" $
      forAll genInt $ \n ->
        addOneArithAndIntLitDisabled n `shouldBe` n + 1
  describe "addOneFunctionDisabled" $ do
    it "increments by one" $
      forAll genInt $ \n ->
        addOneFunctionDisabled n `shouldBe` n + 1
