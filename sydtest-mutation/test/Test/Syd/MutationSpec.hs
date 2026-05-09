{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Syd.MutationSpec (spec) where

import Data.GenValidity
import qualified Data.Text as T
import Data.Validity.Text ()
import Test.QuickCheck
import Test.Syd
import Test.Syd.Mutation
import Test.Syd.Validity

instance Validity TestId where
  validate (TestId steps) =
    mconcat
      [ declare "steps is non-empty" (not (null steps)),
        decorateList steps $ \(desc, idx) ->
          mconcat
            [ declare "description is non-empty" (not (T.null desc)),
              declare "index is non-negative" (idx >= 0)
            ]
      ]

instance GenValid TestId where
  genValid = TestId <$> listOf1 genStep
    where
      genStep = (,) . T.pack <$> listOf1 arbitraryUnicodeChar <*> (getNonNegative <$> arbitrary)
  shrinkValid (TestId steps) =
    [ TestId steps'
    | steps' <- shrinkList shrinkStep steps,
      not (null steps')
    ]
    where
      shrinkStep (desc, idx) =
        [(desc', idx) | desc' <- filter (not . T.null) (map T.pack (shrink (T.unpack desc)))]
          ++ [(desc, idx') | idx' <- filter (>= 0) (shrink idx)]

spec :: Spec
spec = do
  genValidSpec @TestId

  describe "renderTestId" $ do
    it "dot-separates steps" $
      renderTestId (TestId [("foo", 0), ("bar", 0)])
        `shouldBe` "foo.bar"
    it "appends :n when index is nonzero" $
      renderTestId (TestId [("foo", 0), ("bar", 2)])
        `shouldBe` "foo.bar:2"
    it "shows all nonzero indices" $
      renderTestId (TestId [("foo", 1), ("bar", 2)])
        `shouldBe` "foo:1.bar:2"
    it "handles a single step" $
      renderTestId (TestId [("works", 0)])
        `shouldBe` "works"
    it "handles a single step with nonzero index" $
      renderTestId (TestId [("works", 3)])
        `shouldBe` "works:3"
    it "escapes dots in descriptions" $
      renderTestId (TestId [("foo.bar", 0)])
        `shouldBe` "foo\\.bar"
    it "escapes backslashes in descriptions" $
      renderTestId (TestId [("foo\\bar", 0)])
        `shouldBe` "foo\\\\bar"
    it "escapes colons in descriptions" $
      renderTestId (TestId [("foo:bar", 0)])
        `shouldBe` "foo\\:bar"
    it "escapes a trailing colon-digits suffix in descriptions" $
      renderTestId (TestId [("foo:42", 0)])
        `shouldBe` "foo\\:42"

  describe "parseTestIdFilterArg" $ do
    it "returns Nothing on empty input" $
      parseTestIdFilterArg "" `shouldBe` Nothing
    it "parses a single step with no index" $
      parseTestIdFilterArg "foo"
        `shouldBe` Just (TestId [("foo", 0)])
    it "parses a single step with an explicit zero index" $
      parseTestIdFilterArg "foo:0"
        `shouldBe` Just (TestId [("foo", 0)])
    it "parses a single step with a nonzero index" $
      parseTestIdFilterArg "foo:2"
        `shouldBe` Just (TestId [("foo", 2)])
    it "parses multiple steps" $
      parseTestIdFilterArg "foo.bar.baz"
        `shouldBe` Just (TestId [("foo", 0), ("bar", 0), ("baz", 0)])
    it "parses multiple steps with mixed indices" $
      parseTestIdFilterArg "foo.bar:1.baz"
        `shouldBe` Just (TestId [("foo", 0), ("bar", 1), ("baz", 0)])
    it "parses an index on the first step" $
      parseTestIdFilterArg "foo:3.bar.baz"
        `shouldBe` Just (TestId [("foo", 3), ("bar", 0), ("baz", 0)])
    it "parses indices on all steps" $
      parseTestIdFilterArg "foo:1.bar:2.baz:3"
        `shouldBe` Just (TestId [("foo", 1), ("bar", 2), ("baz", 3)])
    it "treats a colon followed by non-digits as part of the description" $
      parseTestIdFilterArg "foo:bar"
        `shouldBe` Just (TestId [("foo:bar", 0)])
    it "parses an escaped dot as part of the description" $
      parseTestIdFilterArg "foo\\.bar"
        `shouldBe` Just (TestId [("foo.bar", 0)])
    it "parses an escaped backslash as part of the description" $
      parseTestIdFilterArg "foo\\\\bar"
        `shouldBe` Just (TestId [("foo\\bar", 0)])
    it "parses an escaped colon as part of the description" $
      parseTestIdFilterArg "foo\\:bar"
        `shouldBe` Just (TestId [("foo:bar", 0)])
    it "returns Nothing on a trailing backslash" $
      parseTestIdFilterArg "foo\\" `shouldBe` Nothing

  describe "renderTestId / parseTestIdFilterArg roundtrip" $
    it "roundtrips valid TestIds" $
      forAllValid $ \tid ->
        parseTestIdFilterArg (renderTestId tid) `shouldBe` Just tid
