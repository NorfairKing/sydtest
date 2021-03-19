{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Process.TypedSpec (spec) where

import Control.Monad.STM
import System.Process.Typed
import Test.Syd
import Test.Syd.Process.Typed

spec :: Spec
spec = typedProcessSpec (setStdout byteStringOutput $ shell "echo hi") $ do
  it "can communicate with the process" $ \ph -> do
    stdout <- atomically $ getStdout ph
    stdout `shouldBe` "hi\n"
