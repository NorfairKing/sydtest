{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines all the functions you will use to define your test suite.
module Test.Syd.Def
  ( -- * API Functions

    -- ** Declaring tests
    describe,
    it,
    itWithOuter,
    itWithBoth,
    itWithAll,
    specify,
    specifyWithOuter,
    specifyWithBoth,
    specifyWithAll,

    -- ** Rexports
    module Test.Syd.Def.TestDefM,
    module Test.Syd.Def.Around,
    module Test.Syd.Def.AroundAll,
  )
where

import Control.Monad.RWS.Strict
import qualified Data.Text as T
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.Def.Around
import Test.Syd.Def.AroundAll
import Test.Syd.Def.TestDefM
import Test.Syd.HList
import Test.Syd.Run
import Test.Syd.SpecDef

-- | Declare a test group
--
-- === Example usage:
--
-- > describe "addition" $ do
-- >     it "adds 3 to 5 to result in 8" $
-- >         3 + 5 `shouldBe` 8
-- >     it "adds 4 to 7 to result in 11" $
-- >         4 + 7 `shouldBe` 11
describe :: String -> TestDefM a b c -> TestDefM a b c
describe s func = censor ((: []) . DefDescribeNode (T.pack s)) func

-- | Declare a test
--
-- __Note: Don't look at the type signature unless you really have to, just follow the examples.__
--
-- === Example usage:
--
-- ==== Tests without resources
--
-- ===== Pure test
--
-- > describe "addition" $
-- >     it "adds 3 to 5 to result in 8" $
-- >         3 + 5 == 8
--
--
-- ===== IO test
--
-- > describe "readFile and writeFile" $
-- >     it "reads back what it wrote for this example" $ do
-- >         let cts = "hello world"
-- >         let fp = "test.txt"
-- >         writeFile fp cts
-- >         cts' <- readFile fp
-- >         cts' `shouldBe` cts
--
--
-- ===== Pure Property test
--
-- > describe "sort" $
-- >     it "is idempotent" $
-- >         forAllValid $ \ls ->
-- >             sort (sort ls) `shouldBe` (sort (ls :: [Int]))
--
--
-- ===== IO Property test
--
-- > describe "readFile and writeFile" $
-- >     it "reads back what it wrote for any example" $ do
-- >         forAllValid $ \fp ->
-- >             forAllValid $ \cts -> do
-- >                 writeFile fp cts
-- >                 cts' <- readFile fp
-- >                 cts' `shouldBe` cts
--
--
-- ==== Tests with an inner resource
--
-- ===== Pure test
--
-- This is quite a rare use-case but here is an example anyway:
--
-- > before (pure 3) $ describe "addition" $
-- >     it "adds 3 to 5 to result in 8" $ \i ->
-- >         i + 5 == 8
--
--
-- ===== IO test
--
-- This test sets up a temporary directory as an inner resource, and makes it available to each test in the group below.
--
-- > let setUpTempDir func = withSystemTempDir $ \tempDir -> func tempDir
-- > in around setUpTempDir describe "readFile and writeFile" $
-- >     it "reads back what it wrote for this example" $ \tempDir -> do
-- >         let cts = "hello world"
-- >         let fp = tempDir </> "test.txt"
-- >         writeFile fp cts
-- >         cts' <- readFile fp
-- >         cts' `shouldBe` cts
--
--
-- ===== Pure property test
--
-- This is quite a rare use-case but here is an example anyway:
--
-- > before (pure 3) $ describe "multiplication" $
-- >     it "is commutative for 5" $ \i ->
-- >         i * 5 == 5 * 3
--
--
-- ===== IO property test
--
-- > let setUpTempDir func = withSystemTempDir $ \tempDir -> func tempDir
-- > in around setUpTempDir describe "readFile and writeFile" $
-- >     it "reads back what it wrote for this example" $ \tempDir ->
-- >         property $ \cts -> do
-- >             let fp = tempDir </> "test.txt"
-- >             writeFile fp cts
-- >             cts' <- readFile fp
-- >             cts' `shouldBe` cts
it :: forall outers test. (HasCallStack, IsTest test, Arg1 test ~ ()) => String -> test -> TestDefM outers (Arg2 test) ()
it s t = do
  sets <- ask
  let testDef =
        TestDef
          { testDefVal = \supplyArgs ->
              runTest
                t
                sets
                ( \func -> supplyArgs (\_ arg2 -> func () arg2)
                ),
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

-- | Declare a test that uses an outer resource
--
-- === Example usage:
--
-- ==== Tests with an outer resource
--
-- ===== __Pure test__
--
-- This is quite a rare use-case but here is an example anyway:
--
-- > beforeAll (pure 3) $ describe "addition" $
-- >     itWithBoth "adds 3 to 5 to result in 8" $ \i ->
-- >         i + 5 == 8
--
--
-- ===== IO test
--
-- This test sets up a temporary directory as an inner resource, and makes it available to each test in the group below.
--
-- > let setUpTempDir func = withSystemTempDir $ \tempDir -> func tempDir
-- > in aroundAll setUpTempDir describe "readFile and writeFile" $
-- >     itWithBoth "reads back what it wrote for this example" $ \tempDir -> do
-- >         let cts = "hello world"
-- >         let fp = tempDir </> "test.txt"
-- >         writeFile fp cts
-- >         cts' <- readFile fp
-- >         cts' `shouldBe` cts
--
--
-- ===== __Pure property test__
--
-- This is quite a rare use-case but here is an example anyway:
--
-- > beforeAll (pure 3) $ describe "multiplication" $
-- >     itWithBoth "is commutative for 5" $ \i ->
-- >         i * 5 == 5 * 3
--
--
-- ===== IO property test
--
-- > let setUpTempDir func = withSystemTempDir $ \tempDir -> func tempDir
-- > in aroundAll setUpTempDir describe "readFile and writeFile" $
-- >     itWithBoth "reads back what it wrote for this example" $ \tempDir ->
-- >         property $ \cts -> do
-- >             let fp = tempDir </> "test.txt"
-- >             writeFile fp cts
-- >             cts' <- readFile fp
-- >             cts' `shouldBe` cts
itWithOuter :: (HasCallStack, IsTest test) => String -> test -> TestDefM (Arg2 test ': l) (Arg1 test) ()
itWithOuter s t = do
  sets <- ask
  let testDef =
        TestDef
          { testDefVal = \supplyArgs ->
              runTest
                t
                sets
                (\func -> supplyArgs $ \(HCons outerArgs _) innerArg -> func innerArg outerArgs),
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

-- | Declare a test that uses both an inner and an outer resource
--
-- === Example usage:
--
-- ==== Tests with both an inner and an outer resource
--
-- ===== __Pure test__
--
-- This is quite a rare use-case but here is an example anyway:
--
-- > beforeAll (pure 3) $ before (pure 5) $ describe "addition" $
-- >     itWithBoth "adds 3 to 5 to result in 8" $ \i j ->
-- >         i + j == 8
--
--
-- ===== IO test
--
-- This test sets up a temporary directory as an inner resource, and makes it available to each test in the group below.
--
-- > let setUpTempDir func = withSystemTempDir $ \tempDir -> func tempDir
-- > in aroundAll setUpTempDir describe "readFile and writeFile" $ before (pure "hello world") $
-- >     itWithBoth "reads back what it wrote for this example" $ \tempDir cts -> do
-- >         let fp = tempDir </> "test.txt"
-- >         writeFile fp cts
-- >         cts' <- readFile fp
-- >         cts' `shouldBe` cts
--
--
-- ===== __Pure property test__
--
-- This is quite a rare use-case but here is an example anyway:
--
-- > beforeAll (pure 3) $ before (pure 5) $ describe "multiplication" $
-- >     itWithBoth "is commutative" $ \i j ->
-- >         i * j == 5 * 3
--
--
-- ===== IO property test
--
-- > let setUpTempDir func = withSystemTempDir $ \tempDir -> func tempDir
-- > in aroundAll setUpTempDir describe "readFile and writeFile" $ before (pure "test.txt") $
-- >     itWithBoth "reads back what it wrote for this example" $ \tempDir fileName ->
-- >         property $ \cts -> do
-- >             let fp = tempDir </> fileName
-- >             writeFile fp cts
-- >             cts' <- readFile fp
-- >             cts' `shouldBe` cts
itWithBoth :: (HasCallStack, IsTest test) => String -> test -> TestDefM (Arg1 test ': l) (Arg2 test) ()
itWithBoth s t = do
  sets <- ask
  let testDef =
        TestDef
          { testDefVal = \supplyArgs ->
              runTest
                t
                sets
                (\func -> supplyArgs $ \(HCons outerArgs _) innerArg -> func outerArgs innerArg),
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

-- | Declare a test that uses all outer resources
--
-- You will most likely never need this function, but in case you do:
-- Note that this will alwast require a type annotation, along with the @GADTs@ and @ScopedTypeVariables@ extensions.
--
-- === Example usage
--
-- > beforeAll (pure 'a') $ beforeAll (pure 5) $
-- >     itWithAll "example" $
-- >         \(HCons c (HCons i HNil) :: HList '[Char, Int]) () ->
-- >             (c, i) `shouldeBe` ('a', 5)
itWithAll :: (HasCallStack, IsTest test, Arg1 test ~ HList l) => String -> test -> TestDefM l (Arg2 test) ()
itWithAll s t = do
  sets <- ask
  let testDef =
        TestDef
          { testDefVal = \supplyArgs ->
              runTest
                t
                sets
                (\func -> supplyArgs func),
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

-- | A synonym for 'it'
specify :: forall outers test. (HasCallStack, IsTest test, Arg1 test ~ ()) => String -> test -> TestDefM outers (Arg2 test) ()
specify = it

-- | A synonym for 'itWithOuter'
specifyWithOuter :: (HasCallStack, IsTest test) => String -> test -> TestDefM (Arg2 test ': l) (Arg1 test) ()
specifyWithOuter = itWithOuter

-- | A synonym for 'itWithBoth'
specifyWithBoth :: (HasCallStack, IsTest test) => String -> test -> TestDefM (Arg1 test ': l) (Arg2 test) ()
specifyWithBoth = itWithBoth

-- | A synonym for 'itWithAll'
specifyWithAll :: (HasCallStack, IsTest test, Arg1 test ~ HList l) => String -> test -> TestDefM l (Arg2 test) ()
specifyWithAll = itWithAll
