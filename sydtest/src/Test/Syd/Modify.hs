{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines functions for declaring different test settings
module Test.Syd.Modify
  ( -- * Declaring different test settings
    modifyMaxSuccess,
    modifyMaxDiscardRatio,
    modifyMaxSize,
    modifyMaxShrinks,
    modifyRunSettings,
    TestRunSettings (..),

    -- * Declaring parallelism
    sequential,
    parallel,
    withParallelism,
    Parallelism (..),

    -- * Declaring randomisation order
    randomiseExecutionOrder,
    doNotRandomiseExecutionOrder,
    withExecutionOrderRandomisation,
    ExecutionOrderRandomisation (..),

    -- * Declaring flakiness
    flaky,
    notFlaky,
    withFlakiness,
    FlakinessMode (..),
  )
where

import Control.Monad.RWS.Strict
import Test.QuickCheck.IO ()
import Test.Syd.Def
import Test.Syd.Run
import Test.Syd.SpecDef

modifyRunSettings :: (TestRunSettings -> TestRunSettings) -> TestDefM a b c -> TestDefM a b c
modifyRunSettings = local

modifyMaxSuccess :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxSuccess func = modifyRunSettings $ \trs -> trs {testRunSettingMaxSuccess = func (testRunSettingMaxSuccess trs)}

modifyMaxDiscardRatio :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxDiscardRatio func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

modifyMaxSize :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxSize func = modifyRunSettings $ \trs -> trs {testRunSettingMaxSize = func (testRunSettingMaxSize trs)}

modifyMaxShrinks :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxShrinks func = modifyRunSettings $ \trs -> trs {testRunSettingMaxShrinks = func (testRunSettingMaxShrinks trs)}

-- | Declare that all tests below must be run sequentially
sequential :: TestDefM a b c -> TestDefM a b c
sequential = withParallelism Sequential

-- | Declare that all tests below may be run in parallel. (This is the default.)
parallel :: TestDefM a b c -> TestDefM a b c
parallel = withParallelism Parallel

withParallelism :: Parallelism -> TestDefM a b c -> TestDefM a b c
withParallelism p = censor ((: []) . DefParallelismNode p)

randomiseExecutionOrder :: TestDefM a b c -> TestDefM a b c
randomiseExecutionOrder = withExecutionOrderRandomisation RandomiseExecutionOrder

doNotRandomiseExecutionOrder :: TestDefM a b c -> TestDefM a b c
doNotRandomiseExecutionOrder = withExecutionOrderRandomisation DoNotRandomiseExecutionOrder

withExecutionOrderRandomisation :: ExecutionOrderRandomisation -> TestDefM a b c -> TestDefM a b c
withExecutionOrderRandomisation p = censor ((: []) . DefRandomisationNode p)

-- | Mark a test suite as "potentially flaky".
--
-- This will retry any test in the given test group up to the given number of tries, and pass a test if it passes once.
-- The test output will show which tests were flaky.
--
-- WARNING: This is only a valid approach to dealing with test flakiness if it is true that tests never pass accidentally.
-- In other words: it must be true that a true positive test failure will fail every time.
flaky :: Int -> TestDefM a b c -> TestDefM a b c
flaky i = withFlakiness $ MayBeFlakyUpTo i

notFlaky :: TestDefM a b c -> TestDefM a b c
notFlaky = withFlakiness MayNotBeFlaky

withFlakiness :: FlakinessMode -> TestDefM a b c -> TestDefM a b c
withFlakiness f = censor ((: []) . DefFlakinessNode f)
