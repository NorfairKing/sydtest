module Main (main) where

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
