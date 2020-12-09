{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.GoldenSpec (spec) where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Rainbow
import Test.Syd

spec :: Spec
spec = do
  describe "outputResultForest" $
    it "outputs the same as last time" $ do
      golden <- SB.readFile "test_resources/output.golden"
      let actual = SB8.intercalate "\n" $ map SB.concat $ outputSpecForestByteString toByteStringsColors256 (Timed [] 0)
      actual `shouldBe` golden
