module Example.ConstFnLibSpec (spec) where

import Example.ConstFnLib
import Test.Syd

spec :: Spec
spec = do
  describe "firstChar" $ do
    it "returns Just the first character of a non-empty string" $
      firstChar "abc" `shouldBe` Just 'a'
    it "returns Nothing for the empty string" $
      firstChar "" `shouldBe` Nothing
  describe "digitsOf" $ do
    it "keeps only the digits" $
      digitsOf "a1b2c3" `shouldBe` "123"
    it "returns the whole string when every character is a digit" $
      digitsOf "42" `shouldBe` "42"
    it "returns the empty string when no character is a digit" $
      digitsOf "abc" `shouldBe` ""
  describe "implies" $ do
    it "False implies anything" $ do
      implies False True `shouldBe` True
      implies False False `shouldBe` True
    it "True implies True" $
      implies True True `shouldBe` True
    it "True does not imply False" $
      implies True False `shouldBe` False
  describe "impliesPair" $ do
    it "rejects (True, False)" $
      impliesPair (True, False) `shouldBe` False
    it "accepts (True, True)" $
      impliesPair (True, True) `shouldBe` True
    it "accepts (False, False)" $
      impliesPair (False, False) `shouldBe` True
  describe "majorityOf5" $ do
    it "is True when 3 of 5 are True" $
      majorityOf5 True True True False False `shouldBe` True
    it "is False when only 2 of 5 are True" $
      majorityOf5 True True False False False `shouldBe` False
    it "is True when all 5 are True" $
      majorityOf5 True True True True True `shouldBe` True
    it "is False when all 5 are False" $
      majorityOf5 False False False False False `shouldBe` False
    -- Exhaustive over all 32 boolean inputs against an independent reference,
    -- so that every per-element mutation (each argument forced True\/False, and
    -- dropping an element from the list) makes some input the swing vote and is
    -- caught.  The reference lives in this (uninstrumented) test package, so it
    -- is not itself mutated.
    it "matches the reference majority on every boolean input" $
      sequence_
        [ majorityOf5 a b c d e `shouldBe` referenceMajority [a, b, c, d, e]
        | a <- bools,
          b <- bools,
          c <- bools,
          d <- bools,
          e <- bools
        ]
  describe "majorityOf5Wrapper" $ do
    it "agrees with majorityOf5 on a 3-out-of-5 tuple" $
      majorityOf5Wrapper (True, True, True, False, False) `shouldBe` True
    it "agrees with majorityOf5 on a 2-out-of-5 tuple" $
      majorityOf5Wrapper (True, True, False, False, False) `shouldBe` False
    it "matches the reference majority on every boolean input" $
      sequence_
        [ majorityOf5Wrapper (a, b, c, d, e) `shouldBe` referenceMajority [a, b, c, d, e]
        | a <- bools,
          b <- bools,
          c <- bools,
          d <- bools,
          e <- bools
        ]
  where
    bools = [False, True]
    -- A majority of a five-element boolean list: at least three Trues.
    referenceMajority xs = length (filter id xs) >= 3
