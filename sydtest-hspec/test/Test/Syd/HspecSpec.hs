module Test.Syd.HspecSpec (spec) where

import Test.Hspec as Hspec
import qualified Test.Syd as Syd
import qualified Test.Syd.Hspec as Syd

spec :: Syd.Spec
spec = Syd.fromHspec exampleHspecSpec

exampleHspecSpec :: Hspec.Spec
exampleHspecSpec = do
  it "adds 3 and 5 together purely" $ 3 + 5 == (8 :: Int)
  it "adds 3 and 5 together in io" $ 3 + 5 `shouldBe` (8 :: Int)
  it "fails here" $ 2 + 2 `shouldBe` (5 :: Int)
