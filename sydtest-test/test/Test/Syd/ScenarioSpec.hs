module Test.Syd.ScenarioSpec (spec) where

import Test.Syd

spec :: Spec
spec = do
  scenarioDir "test_resources/even" $ \rf ->
    it "contains an even number" $ do
      s <- readFile rf
      n <- readIO s
      (n :: Int) `shouldSatisfy` even
  scenarioDirRecur "test_resources/odd" $ \rf ->
    it "contains an odd number" $ do
      s <- readFile rf
      n <- readIO s
      (n :: Int) `shouldSatisfy` odd
