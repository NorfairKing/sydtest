{-# LANGUAGE UnboxedTuples #-}

module Example.TupleSwapLibSpec (spec) where

import Example.TupleSwapLib
import Test.Syd

spec :: Spec
spec = do
  describe "orderedPair" $
    -- Distinct components, so the swapped @(b, a)@ differs, killing the
    -- TupleSwap mutant.
    it "keeps the two components in order" $
      orderedPair 1 2 `shouldBe` (1, 2)

  describe "triple" $
    -- Distinct components, so swapping any pair changes the result and kills
    -- each of the three TupleSwap mutants.
    it "keeps the three components in order" $
      triple 1 2 3 `shouldBe` (1, 2, 3)

  describe "diagonal" $
    it "repeats the single component" $
      diagonal 7 `shouldBe` (7, 7)

  describe "tagChar" $
    it "pairs the number with the character" $
      tagChar 4 'z' `shouldBe` (4, 'z')

  describe "unboxedPair" $
    it "pairs the two numbers in order" $
      case unboxedPair 1 2 of
        (# x, y #) -> (x, y) `shouldBe` (1, 2)
