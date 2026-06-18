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
import Control.Monad (unless, when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Path
import Path.IO (getCurrentDir, setCurrentDir)
import System.Exit (ExitCode (..), die, exitWith)
import System.IO (BufferMode (..), hFlush, hSetBuffering, stderr, stdout)
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
    ControlTally (..),
    MutationRunReport (..),
    MutationTally (..),
    filterAugmentedManifestByIds,
    readAndUnionCoverageDirs,
    writeAugmentedManifestFile,
  )
import Test.Syd.Mutation.Driver.AssertScore (runAssertScore)
import Test.Syd.Mutation.Driver.Components (runInstallComponents, runListComponents)
import Test.Syd.Mutation.Driver.Coverage (runCoverageMode)
import Test.Syd.Mutation.Driver.DiffRun (runDiff)
import Test.Syd.Mutation.Driver.Mutate (runMutationMode)
import Test.Syd.Mutation.Driver.OptParse
import Test.Syd.Mutation.Driver.SuitePkg (walkSuitePkgs)
import Test.Syd.Mutation.Manifest (MutationGroup (..), MutationManifest (..), MutationRecord (..), readManifestDir)
import Test.Syd.Mutation.Runtime (renderMutationId)

-- | Top-level entry point: parse the dispatch and run the chosen
-- subcommand.  The default subcommand is @run@, which runs both mutation
-- phases (coverage then mutation).
sydMutationDriver :: IO ()
sydMutationDriver = do
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
    DispatchDiff settings -> runDiff settings

-- | Run the mutation phase of one instrumented library against pre-computed
-- coverage.
--
-- The @run@ subcommand does NOT gather coverage: coverage is produced
-- separately by the @coverage@ subcommand (one derivation per test package) and
-- handed in via @--coverage-dir@.  'prepareAugmentedFromCoverageDirs' unions
-- those directories (which is where cross-package coverage — a suite in one
-- package covering another package's mutations — is recorded) and restricts the
-- result to this library's mutations.  This keeps coverage a single, shared,
-- non-optional input rather than something each per-library report recomputes.
runDriver :: MutationDriverSettings -> IO ()
runDriver MutationDriverSettings {..} = do
  when (null mutationDriverSettingCoverageDirs) $
    die "sydtest-mutation-driver run: at least one --coverage-dir is required; gather coverage with the 'coverage' subcommand first."
  suites <- walkSuitePkgs mutationDriverSettingSuitePkgs
  prepareAugmentedFromCoverageDirs
    mutationDriverSettingManifests
    mutationDriverSettingCoverageDirs
    mutationDriverSettingAugmentedManifestDir
  -- Mutation phase: 'runMutationMode' spawns each covering suite's mutation
  -- child in that suite's own resource directory (it carries the full
  -- 'SuiteConfig' map, not just the exes), so no parent-side 'cd' is needed
  -- here.  It prints the report to stdout and writes report.json +
  -- report.txt + per-suite *.log files to the out dir.  It returns the run
  -- report; under --fail-fast we then exit non-zero ourselves, preserving
  -- the historical behaviour of the @run@ subcommand (a downstream
  -- @assert-score@ step is the gate when --fail-fast is off).
  report <-
    runMutationMode
      mutationDriverSettingFailFast
      mutationDriverSettingDebug
      mutationDriverSettingAugmentedManifestDir
      mutationDriverSettingOutDir
      mutationDriverSettingChildMemLimit
      mutationDriverSettingMutationJobs
      suites
  hFlush stdout
  when
    ( mutationDriverSettingFailFast
        && ( mutationTallySurvived (mutationRunReportMutations report) > 0
               || mutationTallyUncovered (mutationRunReportMutations report) > 0
               || controlTallyFailed (mutationRunReportControls report) > 0
           )
    )
    $ exitWith (ExitFailure 1)

-- | Assemble the augmented manifest the mutation phase reads from pre-computed
-- per-package coverage directories, instead of running the coverage phase.
--
-- Each coverage directory is produced by the @coverage@ subcommand run for ONE
-- test package against ALL instrumented libraries, so its augmented manifest
-- already records cross-package coverage (a test in that package covering a
-- mutation in another package).  Unioning every package's manifest therefore
-- yields the complete which-test-covers-which-mutation map across packages —
-- which the in-process coverage phase, accumulating suites within a single run,
-- failed to capture for the per-library report.
--
-- The union spans every instrumented library's mutations, so it is restricted
-- to the mutations declared in this run's @--manifest@ directories (one
-- library), keeping the per-library report split.  Uncovered mutations are
-- retained (the coverage manifest carries every mutation, with an empty
-- covering-test set when no suite reached it), so the mutation phase still
-- reports them as uncovered — but only because every coverage directory
-- enumerated every library's mutations.  If a coverage directory is stale or
-- partial, a library mutation can be absent from the union, and restricting to
-- it would silently drop that mutation from the report instead of reporting it
-- uncovered.  Rather than under-report, 'die' when any of this library's
-- mutations is missing from the union.
prepareAugmentedFromCoverageDirs ::
  -- | This library's mutation manifest directories (@--manifest@)
  [Path Abs Dir] ->
  -- | Pre-computed per-package coverage directories (@--coverage-dir@)
  [Path Abs Dir] ->
  -- | Where to write the assembled augmented manifest
  Path Abs Dir ->
  IO ()
prepareAugmentedFromCoverageDirs manifestDirs coverageDirs augDir = do
  unioned@(AugmentedManifest unionedGroups) <- readAndUnionCoverageDirs coverageDirs
  MutationManifest groups <- mconcat <$> mapM readManifestDir manifestDirs
  let libIds = Set.fromList [mutRecId rec | MutationGroup recs <- groups, rec <- recs]
      unionIds =
        Set.fromList
          [augmentedMutationRecordId rec | AugmentedMutationGroup recs <- unionedGroups, rec <- recs]
      missing = Set.toAscList (libIds `Set.difference` unionIds)
  unless (null missing) $
    die $
      unlines $
        ( "sydtest-mutation-driver run: "
            ++ show (length missing)
            ++ " of this library's mutations are absent from the --coverage-dir union; "
            ++ "the coverage directories are stale or do not cover this library. "
            ++ "Re-gather coverage with the 'coverage' subcommand. Missing mutation ids:"
        )
          : map (("  " ++) . renderMutationId) missing
  writeAugmentedManifestFile augDir (filterAugmentedManifestByIds libIds unioned)

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

-- | Run the coverage phase for one suite, with optional @cd@ into its
-- resource directory beforehand.
runOneSuiteCoverage ::
  [Path Abs Dir] ->
  Path Abs Dir ->
  Maybe Word ->
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
