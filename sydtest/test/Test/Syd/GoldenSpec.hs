{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.GoldenSpec (spec) where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Path
import Path.IO
import Rainbow
import Test.Syd

spec :: Spec
spec = do
  describe "outputResultForest" $
    it "outputs the same as last time" $ do
      let goldenFileName = "test_resources/output.golden"
      goldenFile <- resolveFile' goldenFileName
      mGolden <- forgivingAbsence $ SB.readFile $ fromAbsFile goldenFile
      let actual = SB8.intercalate "\n" $ map SB.concat $ outputSpecForestByteString toByteStringsColors256 (Timed [] 0)
      case mGolden of
        Nothing -> do
          ensureDir $ parent goldenFile
          SB.writeFile (fromAbsFile goldenFile) actual
        Just golden -> actual `shouldBe` golden
