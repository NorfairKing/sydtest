{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd
  ( sydTest,
    sydTestWith,
    Settings (..),
    Parallelism (..),
    module Test.Syd.Runner,
    module Test.Syd.Run,
    module Test.Syd.SpecForest,
    module Test.Syd.Expectation,
    module Test.Syd.Output,
    module Test.Syd.Silence,
    module Test.Syd.Def,
  )
where

import Control.Monad
import System.Exit
import Test.QuickCheck.IO ()
import Test.Syd.Def
import Test.Syd.Expectation
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Runner
import Test.Syd.Silence
import Test.Syd.SpecForest

-- | Evaluate a test suite definition and then run it, with default 'Settings'
sydTest :: Spec -> IO ()
sydTest spec = do
  sets <- getSettings
  sydTestWith sets spec

-- | Evaluate a test suite definition and then run it, with given 'Settings'
sydTestWith :: Settings -> Spec -> IO ()
sydTestWith sets spec = do
  resultForest <- sydTestResult sets spec
  when (shouldExitFail resultForest) (exitWith (ExitFailure 1))
