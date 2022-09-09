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

    -- * Modifying the number of retries
    modifyRetries,
    withoutRetries,
    withRetries,

    -- * Declaring flakiness
    flaky,
    flakyWith,
    notFlaky,
    potentiallyFlaky,
    potentiallyFlakyWith,
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
modifyRunSettings func = local (\tde -> tde {testDefEnvTestRunSettings = func $ testDefEnvTestRunSettings tde})

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

-- | Annotate a test group with 'Parallelism'.
withParallelism :: Parallelism -> TestDefM a b c -> TestDefM a b c
withParallelism p = censor ((: []) . DefParallelismNode p)

-- | Declare that the order of execution of all tests below must not be randomised.
doNotRandomiseExecutionOrder :: TestDefM a b c -> TestDefM a b c
doNotRandomiseExecutionOrder = withExecutionOrderRandomisation DoNotRandomiseExecutionOrder

-- | Declare that the order of execution of all tests below may be randomised.
randomiseExecutionOrder :: TestDefM a b c -> TestDefM a b c
randomiseExecutionOrder = withExecutionOrderRandomisation RandomiseExecutionOrder

-- | Annotate a test group with 'ExecutionOrderRandomisation'.
withExecutionOrderRandomisation :: ExecutionOrderRandomisation -> TestDefM a b c -> TestDefM a b c
withExecutionOrderRandomisation p = censor ((: []) . DefRandomisationNode p)

-- | Modify the number of retries to use in flakiness diagnostics.
modifyRetries :: (Word -> Word) -> TestDefM a b c -> TestDefM a b c
modifyRetries modRetries = censor ((: []) . DefRetriesNode modRetries)

-- | Turn off retries
withoutRetries :: TestDefM a b c -> TestDefM a b c
withoutRetries = modifyRetries (const 0)

-- | Make the number of retries this constant
withRetries :: Word -> TestDefM a b c -> TestDefM a b c
withRetries w = modifyRetries (const w)

-- | Mark a test suite as "potentially flaky".
--
-- This will retry any test in the given test group up to the given number of tries, and pass a test if it passes once.
-- The test output will show which tests were flaky.
--
-- WARNING: This is only a valid approach to dealing with test flakiness if it is true that tests never pass accidentally.
-- In other words: tests using flaky must be guaranteed to fail every time if
-- an error is introduced in the code, it should only be added to deal with
-- accidental failures, never accidental passes.
flaky :: Word -> TestDefM a b c -> TestDefM a b c
flaky i = modifyRetries (const i) . withFlakiness (MayBeFlaky Nothing)

-- | Like 'flaky', but also shows the given message to the user whenever the test is flaky.
--
-- You could use it like this:
--
-- >>> flakyWith 3 "Something sometimes goes wrong with the database, see issue 6346" ourTestSuite
flakyWith :: Word -> String -> TestDefM a b c -> TestDefM a b c
flakyWith i message = modifyRetries (const i) . withFlakiness (MayBeFlaky (Just message))

-- | Mark a test suite as "must not be flaky".
--
-- This is useful to have a subgroup of a group marked as 'flaky' that must not be flaky afteral.
notFlaky :: TestDefM a b c -> TestDefM a b c
notFlaky = withFlakiness MayNotBeFlaky

-- | Mark a test suite as 'potentially flaky', such that it will not fail if it is
-- flaky but passes at least once.
potentiallyFlaky :: TestDefM a b c -> TestDefM a b c
potentiallyFlaky = withFlakiness (MayBeFlaky Nothing)

-- | Like 'potentiallyFlaky', but with a message.
potentiallyFlakyWith :: String -> TestDefM a b c -> TestDefM a b c
potentiallyFlakyWith message = withFlakiness (MayBeFlaky (Just message))

-- | Annotate a test group with 'FlakinessMode'.
withFlakiness :: FlakinessMode -> TestDefM a b c -> TestDefM a b c
withFlakiness f = censor ((: []) . DefFlakinessNode f)
