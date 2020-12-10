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
  describe "outputResultForest" $ do
    it "outputs the same as last time" $ do
      let goldenFileName = "test_resources/output.golden"
      goldenFile <- resolveFile' goldenFileName :: IO (Path Abs File)
      pure $
        GoldenTest
          { goldenTestRead = forgivingAbsence $ SB.readFile $ fromAbsFile goldenFile,
            goldenTestProduce = pure $ SB8.intercalate "\n" $ map SB.concat $ outputSpecForestByteString toByteStringsColors256 (Timed [] 0),
            goldenTestWrite = \actual -> do
              ensureDir $ parent goldenFile
              SB.writeFile (fromAbsFile goldenFile) actual,
            goldenTestCompare = (==)
          }
