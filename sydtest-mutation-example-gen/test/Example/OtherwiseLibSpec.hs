module Example.OtherwiseLibSpec (spec) where

import Example.OtherwiseLib
import Test.Syd

spec :: Spec
spec =
  describe "classify" $ do
    it "classifies negative numbers" $
      classify (-1) `shouldBe` "negative"
    it "classifies zero" $
      classify 0 `shouldBe` "zero"
    it "classifies positive numbers" $
      classify 1 `shouldBe` "positive"
