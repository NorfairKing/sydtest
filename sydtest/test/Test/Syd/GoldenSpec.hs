{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.GoldenSpec (spec) where

import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB
import Test.Syd
import Test.Syd.OptParse
import Text.Colour

spec :: Spec
spec = do
  describe "outputResultForest" $ do
    it "outputs the same as last time" $ do
      pureGoldenByteStringFile
        "test_resources/output.golden"
        (LB.toStrict $ SBB.toLazyByteString $ renderResultReport With24BitColours (Timed [] 0))
  describe "defaultSettings" $ do
    it "is the same thing as last time" $ goldenPrettyShowInstance "test_resources/defaultSettings-show.golden" defaultSettings
