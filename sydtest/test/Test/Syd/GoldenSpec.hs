{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.GoldenSpec (spec) where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Rainbow
import Test.Syd

spec :: Spec
spec = do
  describe "outputResultForest" $ do
    it "outputs the same as last time" $ do
      pureGoldenByteStringFile
        "test_resources/output.golden"
        (SB8.intercalate "\n" $ map SB.concat $ outputSpecForestByteString toByteStringsColors256 (Timed [] 0))
  describe "defaultSettings" $ do
    it "is the same thing as last time" $ goldenPrettyShowInstance "test_resources/defaultSettings-show.golden" defaultSettings
