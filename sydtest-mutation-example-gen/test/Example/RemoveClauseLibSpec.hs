module Example.RemoveClauseLibSpec (spec) where

import Example.RemoveClauseLib
import Test.Syd

spec :: Spec
spec =
  describe "describeSize" $ do
    it "describes empty lists" $
      describeSize ([] :: [Int]) `shouldBe` Empty
    it "describes singleton lists" $
      describeSize [1 :: Int] `shouldBe` Single
    it "describes longer lists" $
      describeSize [1, 2 :: Int] `shouldBe` Many
