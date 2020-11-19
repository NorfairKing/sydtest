{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main where

import Control.Exception
import System.Exit
import Test.Syd

data DangerousRecord = Cons1 {field :: String} | Cons2

main :: IO ()
main = do
  let success :: Test -> IO ()
      success test = do
        res <- runTest test
        case res of
          TestPassed -> pure ()
          TestFailed -> die "failed"
  let failed :: Test -> IO ()
      failed test = do
        res <- (Right <$> runTest test) `catch` (\(e :: SomeException) -> pure (Left e))
        case res of
          Right TestPassed -> die "succeeded"
          Right TestFailed -> pure ()
          Left e -> die $ unlines ["Uncaught exception: ", displayException e]
  success (pure ()) -- Something that passes
  failed (error "hi") -- Something that errors in IO
  failed (pure undefined) -- Something that errors purely
  failed (exitWith $ ExitFailure 1) -- Exit code
  failed (throw $ RecConError "test")
  failed (pure (seq (let c = Cons1 {} in field c) ())) -- Record construction (1)
  failed (seq (let c = Cons1 {} in field c) (pure ())) -- Record construction (2)
  failed (throw $ RecSelError "test")
  failed (pure (seq (let c = Cons2 in field c) ())) -- Record selection (1)
  failed (seq (let c = Cons2 in field c) (pure ())) -- Record selection (2)
  failed (throw $ RecUpdError "test")
  failed (pure (seq (let c = Cons2 in c {field = "this will throw"}) ())) -- Record update (1)
  failed (seq (let c = Cons2 in c {field = "this will throw"}) (pure ())) -- Record update (2)
  failed (throw $ PatternMatchFail "test")
  failed (pure (seq (let Cons1 s = Cons2 in s) ())) -- Pattern match (1)
  failed (seq (let Cons1 s = Cons2 in s) (pure ())) -- Pattern match (2)
