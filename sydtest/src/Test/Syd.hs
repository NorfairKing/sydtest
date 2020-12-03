{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

module Test.Syd
  ( -- * Top level API functions
    sydTest,
    sydTestWith,
    Settings (..),
    defaultSettings,
    Parallelism (..),

    -- * Defining a test suite

    -- * Declaring tests
    describe,
    it,
    specify,

    -- ** Declaring test dependencies

    -- *** Dependencies around all of a group of tests
    beforeAll,
    beforeAll_,
    afterAll,
    afterAll_,
    aroundAll,
    aroundAll_,
    aroundAllWith,

    -- *** Dependencies around each of a group of tests
    before,
    before_,
    after,
    after_,
    around,
    around_,
    aroundWith,

    -- *** Declaring different test settings
    modifyMaxSuccess,
    modifyMaxDiscardRatio,
    modifyMaxSize,
    modifyMaxShrinks,
    modifyRunSettings,

    -- ** Test definition types
    TestDefM (..),
    execTestDefM,
    runTestDefM,

    -- ** Test suite types
    TestDef (..),
    TestForest,
    TestTree,
    SpecDefForest,
    SpecDefTree (..),
    ResultForest,
    ResultTree,
    shouldExitFail,

    -- ** Hspec synonyms
    Spec,
    SpecWith,
    SpecM,

    -- * Defining tests

    -- * Reexports
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
