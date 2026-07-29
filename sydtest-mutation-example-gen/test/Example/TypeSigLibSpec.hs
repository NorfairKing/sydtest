module Example.TypeSigLibSpec (spec) where

import Example.TypeSigLib
import Test.Syd

spec :: Spec
spec =
  describe "diffOf" $
    it "subtracts the second number from the first" $
      diffOf 5 2 `shouldBe` 3
