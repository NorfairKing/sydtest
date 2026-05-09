module Example.CaseLibSpec (spec) where

import Example.CaseLib
import Test.Syd

spec :: Spec
spec =
  describe "describeList" $ do
    it "describes empty lists" $
      describeList ([] :: [Int]) `shouldBe` "empty"
    it "describes singleton lists" $
      describeList [1 :: Int] `shouldBe` "singleton"
    it "describes longer lists" $
      describeList [1, 2 :: Int] `shouldBe` "longer"
