module Example.LibSpec (spec) where

import Example.Gen
import Example.Lib
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec =
  describe "addOne" $ do
    it "increments by one" $
      forAll genInt $ \n ->
        addOne n `shouldBe` n + 1
    it "is greater than its input" $
      forAll genInt $ \n ->
        addOne n > n
