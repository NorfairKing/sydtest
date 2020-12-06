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
    it',
    specify,
    specify',

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
describe :: String -> TestDefM a b c -> TestDefM a b c
describe s func = censor ((: []) . DefDescribeNode (T.pack s)) func

-- | Declare a test
--
-- === Example usage:
--
-- ==== Pure test
--
-- > describe "addition" $
-- >     it "adds 3 to 5 to result in 8" $
-- >         3 + 5 `shouldBe` 8
--
-- ==== Property test
--
-- > describe "sort" $
-- >     it "is idempotent" $
-- >         forAllValid $ \ls ->
-- >             sort (sort ls) `shouldBe` (sort (ls :: [Int]))
it :: forall test. (HasCallStack, IsTest test, Arg1 test ~ HList '[]) => String -> test -> TestDefM '[] (Arg2 test) ()
it s t = do
  sets <- ask
  let testDef =
        TestDef
          { testDefVal = \supplyArgs ->
              runTest
                t
                sets
                ( \func -> supplyArgs func
                ),
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

it' :: (HasCallStack, IsTest test, Arg1 test ~ HList l) => String -> test -> TestDefM l (Arg2 test) ()
it' s t = do
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
specify :: (HasCallStack, IsTest test, Arg1 test ~ HList '[]) => String -> test -> TestDefM '[] (Arg2 test) ()
specify = it

-- | A synonym for 'it''
specify' :: (HasCallStack, IsTest test, Arg1 test ~ HList l) => String -> test -> TestDefM l (Arg2 test) ()
specify' = it'
