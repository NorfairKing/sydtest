{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Process.TypedSpec (spec) where

import qualified Data.ByteString as SB
import System.IO
import System.Process.Typed
import Test.Syd
import Test.Syd.Process.Typed

spec :: Spec
spec = typedProcessSpec (setStdin createPipe $ setStdout createPipe $ setStderr inherit $ proc "cat" []) $ do
  -- This test fails, see
  -- https://github.com/fpco/typed-process/issues/38
  xit "can communicate with the process" $ \ph -> do
    let inHandle = getStdin ph
    let outHandle = getStdout ph
    let contents = "Hello world"
    hSetBuffering inHandle NoBuffering
    hSetBuffering outHandle NoBuffering
    SB.hPut inHandle contents
    hFlush inHandle
    output <- SB.hGet outHandle (SB.length contents)
    output `shouldBe` contents
