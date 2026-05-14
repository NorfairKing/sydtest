module Example.LogicLibSpec (spec) where

import Example.LogicLib
import Test.Syd

spec :: Spec
spec = do
  describe "bothPositive" $ do
    it "is True when both are positive" $
      bothPositive 1 1 `shouldBe` True
    it "is False when only the first is positive" $
      bothPositive 1 0 `shouldBe` False
    it "is False when only the second is positive" $
      bothPositive 0 1 `shouldBe` False
    it "is False when neither is positive" $
      bothPositive 0 0 `shouldBe` False
  describe "eitherPositive" $ do
    it "is True when both are positive" $
      eitherPositive 1 1 `shouldBe` True
    it "is True when only the first is positive" $
      eitherPositive 1 0 `shouldBe` True
    it "is True when only the second is positive" $
      eitherPositive 0 1 `shouldBe` True
    it "is False when neither is positive" $
      eitherPositive 0 0 `shouldBe` False
