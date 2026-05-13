module Example.LibSpec (spec) where

import Example.Lib
import Test.Syd

spec :: Spec
spec = do
  describe "addOne" $ do
    it "returns 2 for input 1" $
      addOne 1 `shouldBe` 2
    it "returns 6 for input 5" $
      addOne 5 `shouldBe` 6
