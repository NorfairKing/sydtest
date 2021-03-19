{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Test.Syd.ProcessSpec (spec) where

import qualified Data.ByteString as SB
import System.IO
import System.Process
import Test.Syd
import Test.Syd.Process

spec :: Spec
spec = processSpec ((proc "cat" []) {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}) $
  it "can communicate with the process" $ \(Just inHandle, Just outHandle, _, _) -> do
    let contents = "Hello world"
    hSetBuffering inHandle NoBuffering
    hSetBuffering outHandle NoBuffering
    SB.hPut inHandle contents
    hFlush inHandle
    output <- SB.hGet outHandle (SB.length contents)
    output `shouldBe` contents
