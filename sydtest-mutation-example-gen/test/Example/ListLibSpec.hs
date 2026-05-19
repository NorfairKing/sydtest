module Example.ListLibSpec (spec) where

import Example.ListLib
import Test.Syd

spec :: Spec
spec = do
  describe "pairConcat" $ do
    it "concatenates both arguments" $
      pairConcat "ab" "cd" `shouldBe` "abcd"
    it "preserves the order" $
      pairConcat "xy" "z" `shouldBe` "xyz"
  describe "tripleConcat" $ do
    it "concatenates all three arguments" $
      tripleConcat "a" "b" "c" `shouldBe` "abc"
    it "distinguishes the first element" $
      tripleConcat "X" "" "" `shouldBe` "X"
    it "distinguishes the last element" $
      tripleConcat "" "" "Y" `shouldBe` "Y"
