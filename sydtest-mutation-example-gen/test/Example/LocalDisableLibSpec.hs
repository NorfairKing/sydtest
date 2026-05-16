module Example.LocalDisableLibSpec (spec) where

import Example.LocalDisableLib
import Test.Syd

spec :: Spec
spec = do
  describe "withInnerDisabled" $ do
    it "is False when given True" $
      withInnerDisabled True `shouldBe` False
    it "is True when given False" $
      withInnerDisabled False `shouldBe` True

  describe "withInnerBoolLitDisabled" $ do
    it "is False when given True" $
      withInnerBoolLitDisabled True `shouldBe` False
    it "is True when given False" $
      withInnerBoolLitDisabled False `shouldBe` True

  describe "withInnerKept" $ do
    it "is False when given True" $
      withInnerKept True `shouldBe` False
    it "is True when given False" $
      withInnerKept False `shouldBe` True

  describe "withBindDisabled" $ do
    it "passes the input through the toggle function (True -> False)" $
      withBindDisabled (pure . not) True `shouldReturn` False
    it "passes the input through the toggle function (False -> True)" $
      withBindDisabled (pure . not) False `shouldReturn` True

  describe "withBindKept" $ do
    it "passes the input through the toggle function (True -> False)" $
      withBindKept (pure . not) True `shouldReturn` False
    it "passes the input through the toggle function (False -> True)" $
      withBindKept (pure . not) False `shouldReturn` True
