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
  -- Exhaustive over all 32 boolean inputs, compared against an inline
  -- reference majority.  A few hand-picked inputs leave the per-element
  -- 'ConstBool' and 'ListLit' mutations on @[a, b, c, d, e]@ alive (no single
  -- input distinguishes every position); enumerating every input kills them,
  -- since constanting or dropping any element changes the majority for some
  -- input.
  describe "majorityOf5" $
    it "matches the reference majority on all 32 inputs" $
      sequence_
        [ majorityOf5 a b c d e `shouldBe` (length (filter id [a, b, c, d, e]) >= 3)
        | a <- [False, True],
          b <- [False, True],
          c <- [False, True],
          d <- [False, True],
          e <- [False, True]
        ]
  describe "majorityOf5Wrapper" $
    it "matches the reference majority on all 32 inputs" $
      sequence_
        [ majorityOf5Wrapper (a, b, c, d, e) `shouldBe` (length (filter id [a, b, c, d, e]) >= 3)
        | a <- [False, True],
          b <- [False, True],
          c <- [False, True],
          d <- [False, True],
          e <- [False, True]
        ]
