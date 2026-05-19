module Example.BoolLibSpec (spec) where

import Example.BoolLib
import Test.Syd

spec :: Spec
spec = do
  describe "wrapTrue" $
    it "is Just True" $
      wrapTrue `shouldBe` Just True

  describe "wrapFalse" $
    it "is Just False" $
      wrapFalse `shouldBe` Just False

  describe "negateWrapped" $ do
    it "negates Just True to Just False" $
      negateWrapped (Just True) `shouldBe` Just False
    it "negates Just False to Just True" $
      negateWrapped (Just False) `shouldBe` Just True
    it "leaves Nothing as Nothing" $
      negateWrapped Nothing `shouldBe` Nothing
