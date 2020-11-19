{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main where

import Control.Exception
import System.Exit
import Test.Syd

data DangerousRecord = Cons1 {field :: String} | Cons2

main :: IO ()
main = sydTest $ do
  it "Passes" $ (pure () :: IO ())
  describe "error" $ do
    it "Pure error" $ (pure (error "foobar") :: IO ())
    it "Impure error" $ (error "foobar" :: IO ())
  describe "undefined" $ do
    it "Pure undefined" $ (pure undefined :: IO ())
    it "Impure undefined" $ (undefined :: IO ())
  it "Exit code" $ exitWith $ ExitFailure 1
  describe "Pure exceptions" $ do
    it "Record construction error" $ (throw $ RecConError "test" :: IO ())
    it "Record construction error" $ (pure (seq (let c = Cons1 {} in field c) ()) :: IO ())
    it "Record construction error" $ (seq (let c = Cons1 {} in field c) (pure ()) :: IO ())
    it "Record selection error" $ (throw $ RecSelError "test" :: IO ())
    it "Record selection error" $ (pure (seq (let c = Cons2 in field c) ()) :: IO ())
    it "Record selection error" $ (seq (let c = Cons2 in field c) (pure ()) :: IO ())
    it "Record update error" $ (throw $ RecUpdError "test" :: IO ())
    it "Record update error" $ (pure (seq (let c = Cons2 in c {field = "this will throw"}) ()) :: IO ())
    it "Record update error" $ (seq (let c = Cons2 in c {field = "this will throw"}) (pure ()) :: IO ())
    it "Pattern matching error" $ (throw $ PatternMatchFail "test" :: IO ())
    it "Pattern matching error" $ (pure (seq (let Cons1 s = Cons2 in s) ()) :: IO ())
    it "Pattern matching error" $ (seq (let Cons1 s = Cons2 in s) (pure ()) :: IO ())
  describe "Printing" $ do
    it "print" $ print "hi"
    it "putStrLn" $ putStrLn "hi"
