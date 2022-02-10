{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.HedgehogSpec (spec) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Syd as Syd
import qualified Test.Syd.Hedgehog as Syd

spec :: Syd.Spec
spec = Syd.fromHedgehogGroup exampleHedgehogGroup

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
