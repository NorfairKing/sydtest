module Example.DoLibSpec (spec) where

import Example.DoLib
import Test.Syd

spec :: Spec
spec =
  describe "greet" $ do
    it "greets loudly" $
      greet True `shouldBe` "Hello!\n"
    it "greets quietly" $
      greet False `shouldBe` "Hello\n"
