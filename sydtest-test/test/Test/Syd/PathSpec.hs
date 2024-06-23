{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.PathSpec (spec) where

import qualified Data.ByteString as SB
import Path
import Path.IO
import Test.Syd
import Test.Syd.Path

spec :: Spec
spec = do
  describe "tempDirSpec" $
    tempDirSpec "sydtest-path" $ do
      it "can write a file to a temporary dir and read it" $ \tdir -> do
        tempFile <- resolveFile tdir "tempfile.dat"
        SB.writeFile (fromAbsFile tempFile) "hello"
        contents <- SB.readFile (fromAbsFile tempFile)
        contents `shouldBe` "hello"
      describe "clean state" $
        doNotRandomiseExecutionOrder $ do
          it "can write a file to a temporary dir" $ \tdir -> do
            tempFile <- resolveFile tdir "tempfile.dat"
            SB.writeFile (fromAbsFile tempFile) "hello"
          it "cannot read a file that hasn't been written" $ \tdir -> do
            tempFile <- resolveFile tdir "tempfile.dat"
            contents <- forgivingAbsence $ SB.readFile (fromAbsFile tempFile)
            contents `shouldBe` Nothing
