{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-warn-missing-methods #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.List
import System.Exit
import Test.QuickCheck
import Test.Syd

data DangerousRecord = Cons1 {field :: String} | Cons2

class ToUnit a where
  toUnit :: a -> ()

instance ToUnit Int -- No implementation on purpose

main :: IO ()
main = void $ sydTestResult $ do
  it "Passes" $ (pure () :: IO ())
  describe "error" $ do
    it "Pure error" $ (pure (error "foobar") :: IO ())
    it "Impure error" $ (error "foobar" :: IO ())
  describe "undefined" $ do
    it "Pure undefined" $ (pure undefined :: IO ())
    it "Impure undefined" $ (undefined :: IO ())
  it "Exit code" $ exitWith $ ExitFailure 1
  describe "exceptions" $ do
    it "Record construction error" $ (throw $ RecConError "test" :: IO ())
    exceptionTest "Record construction error" $ let c = Cons1 {} in field c
    it "Record selection error" $ (throw $ RecSelError "test" :: IO ())
    exceptionTest "Record selection error" $ let c = Cons2 in field c
    it "Record update error" $ (throw $ RecUpdError "test" :: IO ())
    exceptionTest "Record update error" $ let c = Cons2 in c {field = "this will throw"}
    it "Pattern matching error" $ (throw $ PatternMatchFail "test" :: IO ())
    exceptionTest "Pattern matching error" $ let Cons1 s = Cons2 in s
    it "ArithException" $ (throw Underflow :: IO ())
    exceptionTest "Pattern matching error" $ 1 `div` 0
    it "NoMethodError" $ (throw (NoMethodError "test") :: IO ())
    exceptionTest "Pattern matching error" $ toUnit (5 :: Int)
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
  describe "Long running tests"
    $ forM_ [1 :: Int .. 10]
    $ \i ->
      it (concat ["takes a while (", show i, ")"]) $
        threadDelay 100_000
  describe "Diff" $ do
    it "shows nice multi-line diffs" $
      ("foo", replicate 7 "quux", "bar") `shouldBe` ("foofoo", replicate 6 "quux", "baz")
    it "shows nice multi-line diffs" $
      ("foo", [], "bar") `shouldBe` ("foofoo", replicate 6 "quux", "baz")

exceptionTest :: String -> a -> Spec
exceptionTest s a = describe s $ do
  it "fails in IO, as the result" $ (pure (seq a ()) :: IO ())
  it "fails in IO, as the action" $ (seq a (pure ()) :: IO ())
  it "fails in pure code" $ seq a True
