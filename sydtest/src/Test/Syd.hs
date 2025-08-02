{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- |
--
-- = Modern testing of Haskell code using /sydtest/
--
-- For a full overview of features and comparisons, please see [the README](https://github.com/NorfairKing/sydtest#readme).
--
-- == What's in a test
--
-- To use @sydtest@, you don't necessarily need to know the following, but for advanced usage you definitely will.
-- If you're just starting out, you can ignore this section and just follow the examples in the docs below.
--
-- * Every test is an instance of the 'IsTest' type class.
--   A test can be a pure 'Bool', an @IO ()@, a 'GoldenTest', some combination of those, or any type that you can implement 'IsTest' for.
--
--  * @sydtest@ allows you to declare resources for use during your tests.
--    This could be things like a database connection or a server to connect to, for example.
--
--  * Every resource is either an outer resource (set up once for a test group) or an inner resource (set up again for each test).
--
--  * Every 'IsTest' instance defines two associated types, an 'Arg1' type and an 'Arg2' type.
--    These correspond to two function arguments. 'Arg1' corresponds to the first and 'Arg2' corresponds to the second.
--    For example, @IO ()@ is an instance of @IsTest@, but @arg -> IO ()@ and @outerArgs -> innerArg -> IO ()@ are as well.
--
--        * For @outerArgs -> innerArgs -> IO ()@, 'Arg1' is @outerArgs@ and 'Arg2' is innerArgs.
--        * For @arg -> IO ()@, 'Arg1' is '()' and 'Arg2' is @arg@.
--        * For @IO ()@, both 'Arg1' and 'Arg2' are '()'.
--
--  * When using 'it' or 'specify' to define tests, the 'Arg1' and 'Arg2' arguments of the test that you pass in have to correspond to the outer and inner resources of your test suite, respectively.
--
--  * You can declare how to set up or tear down resources using the 'around' and 'aroundAll' functions.
module Test.Syd
  ( -- * Top level API functions
    sydTest,
    sydTestWith,

    -- * Defining a test suite

    -- * Declaring tests
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
    getTestDescriptionPath,

    -- ** Commented-out tests
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

    -- ** Golden tests
    pureGoldenTextFile,
    goldenTextFile,
    pureGoldenByteStringFile,
    goldenByteStringFile,
    pureGoldenLazyByteStringFile,
    goldenLazyByteStringFile,
    pureGoldenByteStringBuilderFile,
    goldenByteStringBuilderFile,
    pureGoldenStringFile,
    goldenStringFile,
    goldenShowInstance,
    goldenPrettyShowInstance,
    goldenContext,
    GoldenTest (..),

    -- ** Scenario tests
    scenarioDir,
    scenarioDirRecur,

    -- ** Expectations
    shouldBe,
    shouldNotBe,
    shouldSatisfy,
    shouldSatisfyNamed,
    shouldNotSatisfy,
    shouldNotSatisfyNamed,
    shouldReturn,
    shouldNotReturn,
    shouldStartWith,
    shouldEndWith,
    shouldContain,
    shouldMatchList,
    expectationFailure,
    context,
    Expectation,
    shouldThrow,
    Selector,
    anyException,
    anyErrorCall,
    errorCall,
    anyIOException,
    anyArithException,

    -- *** String expectations
    stringShouldBe,
    textShouldBe,

    -- *** For throwing raw assertions
    stringsNotEqualButShouldHaveBeenEqual,
    textsNotEqualButShouldHaveBeenEqual,
    bytestringsNotEqualButShouldHaveBeenEqual,
    Assertion (..),

    -- ** Declaring test dependencies

    -- *** Dependencies around all of a group of tests
    beforeAll,
    beforeAll_,
    beforeAllWith,
    afterAll,
    afterAll',
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

    -- **** Setup functions

    -- ***** Creating setup functions
    SetupFunc (..),

    -- ***** Using setup functions

    -- ****** Around
    setupAround,
    setupAroundWith,
    setupAroundWith',

    -- ****** AroundAll
    setupAroundAll,
    setupAroundAllWith,

    -- *** Declaring different test settings
    modifyMaxSuccess,
    modifyMaxDiscardRatio,
    modifyMaxSize,
    modifyMaxShrinks,
    modifyRunSettings,
    TestRunSettings (..),

    -- *** Declaring parallelism
    sequential,
    parallel,
    withParallelism,
    Parallelism (..),

    -- *** Declaring randomisation order
    randomiseExecutionOrder,
    doNotRandomiseExecutionOrder,
    withExecutionOrderRandomisation,
    ExecutionOrderRandomisation (..),

    -- *** Modifying the timeout
    modifyTimeout,
    withoutTimeout,
    withTimeout,

    -- *** Modifying the number of retries
    modifyRetries,
    withoutRetries,
    withRetries,

    -- *** Declaring flakiness
    flaky,
    flakyWith,
    notFlaky,
    potentiallyFlaky,
    potentiallyFlakyWith,
    withFlakiness,
    FlakinessMode (..),

    -- *** Declaring expectations
    expectPassing,
    expectFailing,
    withExpectationMode,
    ExpectationMode (..),

    -- *** Doing IO during test definition
    runIO,

    -- ** Test definition types
    TestDefM (..),
    TestDef,
    execTestDefM,
    runTestDefM,
    IsTest (..),

    -- ** Test suite types
    TDef (..),
    TestForest,
    TestTree,
    SpecDefForest,
    SpecDefTree (..),
    ResultForest,
    ResultTree,
    shouldExitFail,

    -- ** Hspec synonyms

    --
    -- These synonyms have been provided so that you can more or less "just" replace "hspec" by "sydTest" and have things work.
    -- However, hspec does not distinguish between inner and outer resources, so these synonyms assume that all resources are inner resources.
    Spec,
    SpecWith,
    SpecM,

    -- * Utilities
    ppShow,
    pPrint,

    -- * Reexports
    module Test.Syd.Def,
    module Test.Syd.Expectation,
    module Test.Syd.HList,
    module Test.Syd.Modify,
    module Test.Syd.Output,
    module Test.Syd.Run,
    module Test.Syd.Runner,
    module Test.Syd.SpecDef,
    module Test.Syd.SpecForest,
    module Control.Monad.IO.Class,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import System.Exit
import Test.QuickCheck.IO ()
import Test.Syd.Def
import Test.Syd.Expectation
import Test.Syd.HList
import Test.Syd.Modify
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.ReRun
import Test.Syd.Run
import Test.Syd.Runner
import Test.Syd.SVG
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Show.Pretty (pPrint, ppShow)

-- | Evaluate a test suite definition and then run it.
--
-- This function perform option-parsing to construct the 'Settings' and then call 'sydTestWith'.
sydTest :: Spec -> IO ()
sydTest spec = do
  sets <- getSettings
  sydTestWith sets spec

-- | Evaluate a test suite definition and then run it, with given 'Settings'
--
-- This function performs no option-parsing.
sydTestWith :: Settings -> Spec -> IO ()
sydTestWith sets spec = do
  resultForest <- withRerunByReport sets (sydTestResult sets) spec

  when (settingProfile sets) $ writeProfile resultForest

  when (shouldExitFail sets (timedValue resultForest)) (exitWith (ExitFailure 1))

-- | Run a test suite during test suite definition.
--
-- This function only exists for backward compatibility.
-- You can also just use 'liftIO' instead.
runIO :: IO e -> TestDefM a b e
runIO = liftIO
