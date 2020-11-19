{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main where

import Control.Exception
import System.Exit
import Test.Syd

data DangerousRecord = Cons1 {field :: String} | Cons2

main :: IO ()
main = sydTest $ do
  it "Passes" $ pure ()
  describe "error" $ do
    it "Pure error" $ pure (error "foobar")
    it "Impure error" $ error "foobar"
  describe "undefined" $ do
    it "Pure undefined" $ pure undefined
    it "Impure undefined" $ undefined
  it "Exit code" $ exitWith $ ExitFailure 1
  describe "Pure exceptions" $ do
    it "Record construction error" $ throw $ RecConError "test"
    it "Record construction error" $ pure (seq (let c = Cons1 {} in field c) ())
    it "Record construction error" $ seq (let c = Cons1 {} in field c) (pure ())
    it "Record selection error" $ throw $ RecSelError "test"
    it "Record selection error" $ pure (seq (let c = Cons2 in field c) ())
    it "Record selection error" $ seq (let c = Cons2 in field c) (pure ())
    it "Record update error" $ throw $ RecUpdError "test"
    it "Record update error" $ pure (seq (let c = Cons2 in c {field = "this will throw"}) ())
    it "Record update error" $ seq (let c = Cons2 in c {field = "this will throw"}) (pure ())
    it "Pattern matching error" $ throw $ PatternMatchFail "test"
    it "Pattern matching error" $ pure (seq (let Cons1 s = Cons2 in s) ())
    it "Pattern matching error" $ seq (let Cons1 s = Cons2 in s) (pure ())
  describe "Printing" $ do
    it "print" $ print "hi"
    it "putStrLn" $ putStrLn "hi"
