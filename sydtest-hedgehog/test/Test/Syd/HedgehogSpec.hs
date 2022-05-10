{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.HedgehogSpec (spec) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd as Syd
import qualified Test.Syd.Hedgehog as Syd

spec :: Syd.Spec
spec = do
  describe "Adapter" $ Syd.fromHedgehogGroup exampleHedgehogGroup
  describe "reverse" $
    specify "reversing twice is the same as not reversing" $
      property $ do
        xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        reverse (reverse xs) === xs
  setupAroundWith (const (pure 0)) $
    describe "reverse with setup" $ do
      specify "reversing twice is the same as not reversing" $ \i ->
        property $ do
          xs <- forAll $ Gen.list (Range.linear i 100) Gen.alpha
          reverse (reverse xs) === xs
  beforeAll (pure (1 :: Int)) $ do
    describe "reverse with beforeAll" $ do
      specifyWithBoth "reversing twice is the same as not reversing" $ \i () ->
        property $ do
          xs <- forAll $ Gen.list (Range.linear i 100) Gen.alpha
          reverse (reverse xs) === xs

exampleHedgehogGroup :: Hedgehog.Group
exampleHedgehogGroup =
  Hedgehog.Group
    "test group name"
    [ ( "prop_add_distributive_int",
        property $ do
          let genInt :: Gen Int
              genInt = Gen.int Range.linearBounded
          a <- forAll genInt
          b <- forAll genInt
          c <- forAll genInt
          classify "all zero" $ a == 0 && b == 0 && c == 0
          classify "False" False
          classify "a == 0" $ a == 0
          classify "b == 0" $ b == 0
          classify "c == 0" $ c == 0
          let left = a * (b + c)
          let right = (a * b) + (a * c)
          classify "left == 0" $ left == 0
          classify "right == 0" $ right == 0
          left === right
      ),
      ( "prop_add_distributive_double",
        property $ do
          let genDouble :: Gen Double
              genDouble = Gen.double $ Range.exponentialFloat (-1E1024) 1E1024
          a <- forAll genDouble
          b <- forAll genDouble
          c <- forAll genDouble
          a * (b + c) === (a * b) + (a * c)
      ),
      ( "prop_undefined",
        property $ do
          _ <- forAll $ Gen.int Range.linearBounded
          undefined
      ),
      ( "prop_equal",
        property $ do
          a <- forAll $ Gen.int Range.linearBounded
          b <- forAll $ Gen.int Range.linearBounded
          footnote "foobar"
          a === b
      )
    ]
