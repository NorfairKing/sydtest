{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main where

import Control.Exception
import Data.List
import System.Exit
import Test.QuickCheck
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
  describe "Pure exceptions in IO" $ do
    it "Record construction error" $ (throw $ RecConError "test" :: IO ())
    exceptionTest "Record construction error" $ let c = Cons1 {} in field c
    it "Record selection error" $ (throw $ RecSelError "test" :: IO ())
    exceptionTest "Record selection error" $ let c = Cons2 in field c
    it "Record update error" $ (throw $ RecUpdError "test" :: IO ())
    exceptionTest "Record update error" $ let c = Cons2 in c {field = "this will throw"}
    it "Pattern matching error" $ (throw $ PatternMatchFail "test" :: IO ())
    exceptionTest "Pattern matching error" $ let Cons1 s = Cons2 in s
  describe "Printing" $ do
    it "print" $ print "hi"
    it "putStrLn" $ putStrLn "hi"
  describe "Property tests" $ do
    describe "pure" $ do
      it "reversing a list twice is the same as reversing it once"
        $ property
        $ \ls -> reverse (reverse ls) == (ls :: [Int])
      it "should fail to show that sorting does nothing"
        $ property
        $ \ls -> sort ls == (ls :: [Int])
    describe "impure" $ do
      it "reversing a list twice is the same as reversing it once"
        $ property
        $ \ls -> reverse (reverse ls) `shouldBe` (ls :: [Int])
      it "should fail to show that sorting does nothing"
        $ property
        $ \ls -> sort ls `shouldBe` (ls :: [Int])

exceptionTest :: String -> a -> Spec
exceptionTest s a = describe s $ do
  it "fails in IO, as the result" $ (pure (seq a ()) :: IO ())
  it "fails in IO, as the action" $ (seq a (pure ()) :: IO ())
  it "fails in pure code" $ seq a True
