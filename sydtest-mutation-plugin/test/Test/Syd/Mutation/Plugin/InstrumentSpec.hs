{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.InstrumentSpec (spec) where

import qualified Data.Text as T
import GHC.Data.FastString (mkFastString)
import GHC.Types.SrcLoc
import Test.Syd
import Test.Syd.Mutation.Plugin.Instrument

mkSpan :: Int -> Int -> Int -> Int -> RealSrcSpan
mkSpan startLine startCol endLine endCol =
  mkRealSrcSpan
    (mkRealSrcLoc (mkFastString "test.hs") startLine startCol)
    (mkRealSrcLoc (mkFastString "test.hs") endLine endCol)

spec :: Spec
spec = describe "applySpanRemoval" $ do
  it "removes the requested lines from a multi-line outer span" $
    applySpanRemoval
      ["one", "two", "three", "four"]
      1
      4
      [mkSpan 2 1 2 4]
      `shouldBe` ["one", "three", "four"]

  it "does not crash when the outer span extends past the end of allLines" $
    -- This reproduces the crash from a -pgmF preprocessor (e.g. sydtest-discover):
    -- the on-disk source file has only 1 line, but GHC's source spans refer to
    -- the generated source which is many lines longer.
    applySpanRemoval
      ["only one on-disk line"]
      1
      13
      [mkSpan 5 1 5 10]
      `shouldBe` ["only one on-disk line"]

  it "does not crash when outerStart is past the end of allLines" $
    applySpanRemoval
      ["one", "two"]
      10
      15
      [mkSpan 12 1 12 5]
      `shouldBe` []

  it "returns an empty list when there are no lines to keep" $
    applySpanRemoval
      ["one", "two", "three"]
      1
      3
      [mkSpan 1 1 3 5]
      `shouldBe` []

  it "handles multiple removed spans" $
    applySpanRemoval
      (map T.pack ["a", "b", "c", "d", "e"])
      1
      5
      [mkSpan 2 1 2 2, mkSpan 4 1 4 2]
      `shouldBe` ["a", "c", "e"]
