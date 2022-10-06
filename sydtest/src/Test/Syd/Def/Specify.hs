{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | This module defines all the functions you will use to define your test suite.
module Test.Syd.Def.Specify
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
    prop,

    -- ** Declaring commented-out tests
    xdescribe,
    xit,
    xitWithOuter,
    xitWithBoth,
    xitWithAll,
    xspecify,
    xspecifyWithOuter,
    xspecifyWithBoth,
    xspecifyWithAll,

    -- ** Pending tests
    pending,
    pendingWith,
  )
where

import Control.Monad.RWS.Strict
import qualified Data.Text as T
import GHC.Stack
import Test.QuickCheck
import Test.QuickCheck.IO ()
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
describe ::
  -- | The test group description
  String ->
  TestDefM outers inner () ->
  TestDefM outers inner ()
describe s =
  let t = T.pack s
   in local (\tde -> tde {testDefEnvDescriptionPath = t : testDefEnvDescriptionPath tde})
        . censor ((: []) . DefDescribeNode t)

xdescribe :: String -> TestDefM outers inner () -> TestDefM outers inner ()
xdescribe s = describe s . censor (markSpecForestAsPending Nothing)

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
it ::
  forall outers inner test.
  (HasCallStack, IsTest test, Arg1 test ~ (), Arg2 test ~ inner) =>
  -- | The description of the test
  String ->
  -- | The test itself
  test ->
  TestDefM outers inner ()
it s t = withFrozenCallStack $ do
  sets <- asks testDefEnvTestRunSettings
  let testDef =
        TDef
          { testDefVal = \progressReporter supplyArgs ->
              runTest
                t
                sets
                progressReporter
                ( \func -> supplyArgs (\_ arg2 -> func () arg2)
                ),
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

xit ::
  forall outers inner test.
  (HasCallStack, IsTest test, Arg1 test ~ (), Arg2 test ~ inner) =>
  -- | The description of the test
  String ->
  -- | The test itself
  test ->
  TestDefM outers inner ()
xit s _ = pending s

-- | A synonym for 'it'
specify ::
  forall outers inner test.
  (HasCallStack, IsTest test, Arg1 test ~ (), Arg2 test ~ inner) =>
  -- | The description of the test
  String ->
  -- | The test itself
  test ->
  TestDefM outers inner ()
specify s t = withFrozenCallStack $ it s t

-- | A synonym for 'xit'
xspecify ::
  forall outers inner test.
  (HasCallStack, IsTest test, Arg1 test ~ (), Arg2 test ~ inner) =>
  -- | The description of the test
  String ->
  -- | The test itself
  test ->
  TestDefM outers inner ()
xspecify = xit

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
-- >     itWithOuter "adds 3 to 5 to result in 8" $ \i ->
-- >         i + 5 == 8
--
--
-- ===== IO test
--
-- This test sets up a temporary directory as an inner resource, and makes it available to each test in the group below.
--
-- > let setUpTempDir func = withSystemTempDir $ \tempDir -> func tempDir
-- > in aroundAll setUpTempDir describe "readFile and writeFile" $
-- >     itWithOuter "reads back what it wrote for this example" $ \tempDir -> do
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
-- >     itWithOuter "is commutative for 5" $ \i ->
-- >         i * 5 == 5 * 3
--
--
-- ===== IO property test
--
-- > let setUpTempDir func = withSystemTempDir $ \tempDir -> func tempDir
-- > in aroundAll setUpTempDir describe "readFile and writeFile" $
-- >     itWithouter "reads back what it wrote for this example" $ \tempDir ->
-- >         property $ \cts -> do
-- >             let fp = tempDir </> "test.txt"
-- >             writeFile fp cts
-- >             cts' <- readFile fp
-- >             cts' `shouldBe` cts
itWithOuter ::
  (HasCallStack, IsTest test, Arg1 test ~ inner, Arg2 test ~ outer) =>
  -- The test description
  String ->
  -- The test itself
  test ->
  TestDefM (outer ': otherOuters) inner ()
itWithOuter s t = withFrozenCallStack $ do
  sets <- asks testDefEnvTestRunSettings
  let testDef =
        TDef
          { testDefVal = \progressReporter supplyArgs ->
              runTest
                t
                sets
                progressReporter
                (\func -> supplyArgs $ \(HCons outerArgs _) innerArg -> func innerArg outerArgs),
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

xitWithOuter ::
  (HasCallStack, IsTest test, Arg1 test ~ inner, Arg2 test ~ outer) =>
  -- The test description
  String ->
  -- The test itself
  test ->
  TestDefM (outer ': otherOuters) inner ()
xitWithOuter s _ = pending s

-- | A synonym for 'itWithOuter'
specifyWithOuter ::
  (HasCallStack, IsTest test, Arg1 test ~ inner, Arg2 test ~ outer) =>
  -- The test description
  String ->
  -- The test itself
  test ->
  TestDefM (outer ': otherOuters) inner ()
specifyWithOuter s t = withFrozenCallStack $ itWithOuter s t

-- | A synonym for 'xitWithOuter'
xspecifyWithOuter ::
  (HasCallStack, IsTest test, Arg1 test ~ inner, Arg2 test ~ outer) =>
  -- The test description
  String ->
  -- The test itself
  test ->
  TestDefM (outer ': otherOuters) inner ()
xspecifyWithOuter = xitWithOuter

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
itWithBoth ::
  ( HasCallStack,
    IsTest test,
    Arg1 test ~ outer,
    Arg2 test ~ inner
  ) =>
  String ->
  test ->
  TestDefM (outer ': otherOuters) inner ()
itWithBoth s t = withFrozenCallStack $ do
  sets <- asks testDefEnvTestRunSettings
  let testDef =
        TDef
          { testDefVal = \progressReporter supplyArgs ->
              runTest
                t
                sets
                progressReporter
                (\func -> supplyArgs $ \(HCons outerArgs _) innerArg -> func outerArgs innerArg),
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

xitWithBoth ::
  ( HasCallStack,
    IsTest test,
    Arg1 test ~ outer,
    Arg2 test ~ inner
  ) =>
  String ->
  test ->
  TestDefM (outer ': otherOuters) inner ()
xitWithBoth s _ = pending s

-- | A synonym for 'itWithBoth'
specifyWithBoth ::
  ( HasCallStack,
    IsTest test,
    Arg1 test ~ outer,
    Arg2 test ~ inner
  ) =>
  String ->
  test ->
  TestDefM (outer ': otherOuters) inner ()
specifyWithBoth s t = withFrozenCallStack $ itWithBoth s t

-- | A synonym for 'xitWithBoth'
xspecifyWithBoth ::
  ( HasCallStack,
    IsTest test,
    Arg1 test ~ outer,
    Arg2 test ~ inner
  ) =>
  String ->
  test ->
  TestDefM (outer ': otherOuters) inner ()
xspecifyWithBoth = xitWithBoth

-- | Declare a test that uses all outer resources
--
-- You will most likely never need this function, but in case you do:
-- Note that this will always require a type annotation, along with the @GADTs@ and @ScopedTypeVariables@ extensions.
--
-- === Example usage
--
-- > beforeAll (pure 'a') $ beforeAll (pure 5) $
-- >     itWithAll "example" $
-- >         \(HCons c (HCons i HNil) :: HList '[Char, Int]) () ->
-- >             (c, i) `shouldeBe` ('a', 5)
itWithAll ::
  ( HasCallStack,
    IsTest test,
    Arg1 test ~ HList outers,
    Arg2 test ~ inner
  ) =>
  String ->
  test ->
  TestDefM outers inner ()
itWithAll s t = withFrozenCallStack $ do
  sets <- asks testDefEnvTestRunSettings
  let testDef =
        TDef
          { testDefVal = \progressReporter supplyArgs ->
              runTest
                t
                sets
                progressReporter
                (\func -> supplyArgs func),
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

xitWithAll ::
  ( HasCallStack,
    IsTest test,
    Arg1 test ~ HList outers,
    Arg2 test ~ inner
  ) =>
  String ->
  test ->
  TestDefM outers inner ()
xitWithAll s _ = pending s

-- | A synonym for 'itWithAll'
specifyWithAll ::
  ( HasCallStack,
    IsTest test,
    Arg1 test ~ HList outers,
    Arg2 test ~ inner
  ) =>
  String ->
  test ->
  TestDefM outers inner ()
specifyWithAll s t = withFrozenCallStack $ itWithAll s t

-- | A synonym for 'xitWithAll'
xspecifyWithAll ::
  ( HasCallStack,
    IsTest test,
    Arg1 test ~ HList outers,
    Arg2 test ~ inner
  ) =>
  String ->
  test ->
  TestDefM outers inner ()
xspecifyWithAll = xitWithAll

-- | Convenience function for backwards compatibility with @hspec@
--
-- > prop s p = it s $ property p
prop :: Testable prop => String -> prop -> Spec
prop s p = withFrozenCallStack $ it s $ property p

-- | Declare a test that has not been written yet.
pending :: String -> TestDefM outers inner ()
pending s = tell [DefPendingNode (T.pack s) Nothing]

-- | Declare a test that has not been written yet for the given reason.
pendingWith :: String -> String -> TestDefM outers inner ()
pendingWith description reasonWhyItsPending = tell [DefPendingNode (T.pack description) (Just (T.pack reasonWhyItsPending))]
