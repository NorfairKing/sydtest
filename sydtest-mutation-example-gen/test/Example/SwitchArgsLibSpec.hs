module Example.SwitchArgsLibSpec (spec) where

import Example.SwitchArgsLib
import Test.Syd

spec :: Spec
spec = do
  describe "divideThe" $
    -- Asymmetric arguments: the swapped @div b a@ gives a different result,
    -- which kills the SwitchFunctionArguments mutant.
    it "divides the first argument by the second" $
      divideThe 6 2 `shouldBe` 3

  describe "parenDivide" $
    -- The parenthesised @div a b@ must produce exactly one swap mutant, and
    -- the asymmetric @div@ lets this test kill it.
    it "divides the first argument by the second, then succeeds" $
      parenDivide 6 2 `shouldBe` 4

  describe "makeColour" $
    -- Distinct components, so swapping any pair changes the result and kills
    -- each of the three SwitchFunctionArguments mutants.
    it "keeps the components in order" $
      makeColour 1 2 3 `shouldBe` RGB 1 2 3

  describe "greyscale" $
    it "repeats the single component" $
      greyscale 7 `shouldBe` RGB 7 7 7
