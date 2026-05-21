{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Top-level entry point for the @sydtest-mutation-driver@ executable.
--
-- The driver orchestrates the two-phase mutation testing workflow:
--
-- 1. Coverage phase: for each declared suite, spawn the suite executable
--    once per leaf test with @--mutation-coverage-one@ to discover which
--    tests cover which mutations.  Merge the per-suite results into a
--    single @manifest-augmented.json@.
-- 2. Mutation phase: spawn one child per mutation per covering suite;
--    treat exit-zero as "survived" and non-zero as "killed".  Write
--    @report.txt@ and @report.json@ to the configured report directory.
module Test.Syd.Mutation.Driver
  ( sydMutationDriver,
    module Test.Syd.Mutation.Driver.OptParse,
  )
where

import Control.Exception (bracket)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Path
import Path.IO (getCurrentDir, setCurrentDir)
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    mergeAugmentedManifests,
    readAugmentedManifestFile,
    writeAugmentedManifestFile,
  )
import Test.Syd.Mutation.Driver.AssertScore (runAssertScore)
import Test.Syd.Mutation.Driver.Components (runInstallComponents, runListComponents)
import Test.Syd.Mutation.Driver.Coverage (runCoverageMode)
import Test.Syd.Mutation.Driver.DiffRun (runDiff)
import Test.Syd.Mutation.Driver.Mutate (runMutationMode)
import Test.Syd.Mutation.Driver.OptParse
import Test.Syd.Mutation.Driver.SuitePkg (walkSuitePkgs)

-- | Top-level entry point: parse the dispatch and run the chosen
-- subcommand.  The default subcommand is @run@, which runs both mutation
-- phases (coverage then mutation).
sydMutationDriver :: IO ()
sydMutationDriver = do
  -- Set both streams to UTF-8 so the rendered report (which contains
  -- box-drawing characters and em-dashes) doesn't fail to print when
  -- running in a build sandbox where LANG isn't set.
  hSetEncoding stderr utf8
  hSetEncoding stdout utf8
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering
  dispatch <- getDispatch
  case dispatch of
    DispatchRun settings -> runDriver settings
    DispatchListComponents kind cabalFile -> runListComponents kind cabalFile
    DispatchInstallComponents kind cabalFile outDir ->
      runInstallComponents kind cabalFile outDir
    DispatchAssertScore assertNoneUncovered reportDir mOutDir ->
      runAssertScore assertNoneUncovered reportDir mOutDir
    DispatchCoverage settings -> runCoverage settings
    DispatchMergeCoverage settings -> runMergeCoverage settings
    DispatchDiff settings -> runDiff settings

-- | Run the driver phases in order: coverage, then mutation.
runDriver :: MutationDriverSettings -> IO ()
runDriver MutationDriverSettings {..} = do
  suites <- walkSuitePkgs mutationDriverSettingSuitePkgs
  let suitesAsc = Map.toAscList suites
  -- Coverage phase: for each suite, cd into its resource directory (if
  -- set) and run the coverage parent against its exe.  The augmented
  -- manifest accumulates across suites: each suite's pass merges into the
  -- prior augmented manifest.
  mapM_
    ( runOneSuiteCoverage
        mutationDriverSettingManifests
        mutationDriverSettingAugmentedManifestDir
        mutationDriverSettingCoverageJobs
        mutationDriverSettingCoverageRetry
        mutationDriverSettingFailFast
    )
    suitesAsc
  -- Mutation phase: cd into the first suite's resource directory (in
  -- Map.toAscList order, which is sorted by key), then run the mutation
  -- parent.  The child suite-exe map is the same map of declared suites.
  let firstSuiteResourceDir = case suitesAsc of
        ((_, sc) : _) -> suiteConfigResourceDir sc
        [] -> Nothing
      suiteExesByName = Map.map suiteConfigExe suites
  -- 'runMutationMode' prints the report to stdout, writes
  -- report.json + report.txt + per-suite *.log files to the out dir,
  -- and (under --fail-fast) exitWith's directly without returning.
  _ <-
    withMaybeCurrentDir firstSuiteResourceDir $
      runMutationMode
        mutationDriverSettingFailFast
        mutationDriverSettingAugmentedManifestDir
        mutationDriverSettingOutDir
        mutationDriverSettingChildMemLimit
        suiteExesByName
  hFlush stdout

-- | Run only the coverage phase: for each suite, run the coverage parent so
-- the augmented manifest accumulates, then stop.  Writes no report; the
-- augmented manifest it leaves behind is the diff-scoped runner's cache.
runCoverage :: CoverageSettings -> IO ()
runCoverage CoverageSettings {..} = do
  suites <- walkSuitePkgs coverageSettingSuitePkgs
  mapM_
    ( runOneSuiteCoverage
        coverageSettingManifests
        coverageSettingAugmentedManifestDir
        coverageSettingCoverageJobs
        coverageSettingCoverageRetry
        coverageSettingFailFast
    )
    (Map.toAscList suites)
  hFlush stdout

-- | Union several per-suite augmented manifests into one.  Each input was
-- produced by a per-suite 'runCoverage'; 'mergeAugmentedManifests' unions
-- their @coveringTests@ per mutation, so the result is identical to having
-- run coverage for all suites in one derivation.  The merge is associative
-- and the inputs are folded left-to-right with an empty manifest as the
-- identity, so the order of @--input@ flags does not affect the result.
runMergeCoverage :: MergeCoverageSettings -> IO ()
runMergeCoverage MergeCoverageSettings {..} = do
  inputs <- mapM readAugmentedManifestFile mergeCoverageSettingInputs
  let merged = foldl mergeAugmentedManifests (AugmentedManifest []) inputs
  writeAugmentedManifestFile mergeCoverageSettingOutputDir merged
  hFlush stdout

-- | Run the coverage phase for one suite, with optional @cd@ into its
-- resource directory beforehand.
runOneSuiteCoverage ::
  [Path Abs Dir] ->
  Path Abs Dir ->
  Maybe Int ->
  Word ->
  Bool ->
  (Text, SuiteConfig) ->
  IO ()
runOneSuiteCoverage manifests augmentedManifestDir coverageJobs coverageRetry failFast (suiteName, SuiteConfig {suiteConfigExe, suiteConfigResourceDir}) =
  withMaybeCurrentDir suiteConfigResourceDir $
    runCoverageMode
      failFast
      manifests
      augmentedManifestDir
      coverageJobs
      coverageRetry
      suiteName
      suiteConfigExe

-- | Bracket-style @cd@ into the given directory for the duration of the
-- action.  Restores the original working directory afterward.
withMaybeCurrentDir :: Maybe (Path Abs Dir) -> IO a -> IO a
withMaybeCurrentDir = \case
  Nothing -> id
  Just dir -> withCurrentDir' dir

-- | Local copy of 'Path.IO.withCurrentDir'-style bracketing.  Restoring
-- the previous directory in 'bracket' guarantees the restore happens
-- even when the action throws.
withCurrentDir' :: Path Abs Dir -> IO a -> IO a
withCurrentDir' dir action =
  bracket getCurrentDir setCurrentDir $ \_ -> do
    setCurrentDir dir
    action
