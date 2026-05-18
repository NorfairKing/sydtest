{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides four entry points for mutation testing:
--
-- * 'runCoverageMode': parent process — enumerate all tests, spawn one child
--   subprocess per test concurrently to collect coverage, merge results, and
--   write @manifest-augmented.json@.
-- * 'runSingleCoverageMode': child process — run one test, write its
--   'TestCoverageMap' to a file, and exit.
-- * 'runMutationMode': parent process — read the augmented manifest and spawn
--   one child process per mutation.
-- * 'runSingleMutationMode': child process — run only the tests that cover a
--   single mutation and exit with success/failure.
--
-- Running each mutation (and each coverage collection) in a subprocess is
-- necessary for three reasons:
--
-- 1. __Memory limit__: a mutation can cause unbounded allocation (e.g. turning
--    a termination condition into a no-op).  The RTS @-M@ flag terminates the
--    process when the heap cap is hit — this is not a catchable exception.  In
--    a subprocess only the child dies; the parent records it as killed and
--    moves on.
--
-- 2. __Laziness \/ memoisation__: the spec forest is built once and reused
--    across mutations.  Values computed via 'runIO' during spec construction
--    are memoised in the parent\'s heap and will not reflect the active
--    mutation.  Subprocesses get a fresh heap each time, so there is no risk
--    of a memoised value masking a mutation.
--
-- 3. __Parallelism__: because 'activeMutation' and 'coverageSlot' are
--    process-global 'IORef's, both mutations and coverage collection must run
--    serially within a single process.  Subprocesses are independent, so the
--    parent runs up to N children concurrently where N = 'getNumCapabilities'.
module Test.Syd.MutationMode
  ( runMutationMode,
    runCoverageMode,
    runSingleCoverageMode,
    renderMutationRunReport,
    renderMutationProgressEvent,
    resolveAugmentedManifestDir,
    CoverageProgressEvent (..),
    CoverageProgressTestEvent (..),
    CoverageProgressPhase (..),
    CoverageProgressSkipReason (..),
    renderCoverageProgressEvent,
    formatMutationLog,
    renderUnifiedDiff,

    -- * Exposed for testing
    SuiteOutcome (..),
    MutationResult (..),
    classifySyncExceptionAsKilled,
    retryingIO,
    isMutationFailure,
    mutationResultId,
    resultToOutcome,
    runOneGroup,
  )
where

import Control.Concurrent (newQSem, signalQSem, threadDelay, waitQSem)
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (Exception, bracket, bracket_)
import qualified Control.Exception as Exception
import Control.Monad (when)
import Data.IORef
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.Vector as V
import GHC.Conc (getNumCapabilities)
import Myers.Diff (PolyDiff (..), getGroupedDiff, getTextDiff)
import Path
import Path.IO (copyFile, getCurrentDir, ignoringAbsence, withSystemTempDir)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitWith)
import System.IO (BufferMode (..), IOMode (..), hFlush, hSetBuffering, stderr, withFile)
import System.Process.Typed (proc, runProcess, setStderr, setStdout, startProcess, stopProcess, useHandleOpen, waitExitCode)
import Test.Syd.Def
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
    MutationGroupReport (..),
    MutationOutcome (..),
    MutationProgressEvent (..),
    MutationRunReport (..),
    SkippedMutation (..),
    SurvivedMutation (..),
    TimedOutMutation (..),
    UncoveredMutation (..),
    defaultTimeoutMicros,
    fromMutationRecord,
    mergeAugmentedManifests,
    readAugmentedManifestFile,
    readAugmentedManifestFileIfExists,
    writeAugmentedManifestFile,
    writeMutationRunReport,
  )
import Test.Syd.Mutation.Forest (filterTestForestByTrie, flattenTestForestWithIds, testIdTrieFromList)
import Test.Syd.Mutation.Manifest
  ( MutationGroup (..),
    MutationManifest (..),
    MutationRecord (..),
    readManifestDir,
  )
import Test.Syd.Mutation.Runtime (MutationId (..), renderMutationId, withCoverageSlot)
import Test.Syd.Mutation.TestBaselineMap (TestBaselineMap (..), readTestBaselineMapFile, writeTestBaselineMapFile)
import Test.Syd.Mutation.TestCoverageMap (TestCoverageMap (..), readTestCoverageMapFile, writeTestCoverageMapFile)
import Test.Syd.Mutation.TestId (TestId, parseTestIdFilterArg, renderTestId)
import Test.Syd.OptParse
import Test.Syd.Output (printOutputSpecForest)
import Test.Syd.Output.Common (addColour, delColour, renderAddSide, renderDelSide)
import Test.Syd.Run
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef
import Text.Colour (Chunk, chunk, cyan, fore, green, hPutChunksLocaleWith, putChunksLocaleWith, red, unlinesChunks, yellow)

-- | Parent process: enumerate all leaf tests, spawn one coverage child
-- subprocess per test (up to N concurrently, N defaults to
-- 'getNumCapabilities' but can be overridden with @--mutation-coverage-jobs@),
-- merge the resulting 'TestCoverageMap's, and write (or merge into)
-- @manifest-augmented.json@ at the resolved augmented-manifest directory.
--
-- When 'coverageParentSuiteName' is set, covering tests are recorded under
-- that suite name key.  If @manifest-augmented.json@ already exists (from a
-- prior suite's coverage pass), this run's results are merged in so that
-- multiple suites can be run sequentially.
runCoverageMode :: Settings -> Bool -> CoverageParentSettings -> Spec -> IO ()
runCoverageMode settings failFast covParent spec = do
  let manifestDirs = NE.toList (coverageParentManifestDirs covParent)
  -- LineBuffering on stderr so our writes hit the fd at line boundaries,
  -- not when the buffer happens to fill. Coverage children inherit this fd
  -- and write to it directly (bypassing our Handle's MVar lock); if our own
  -- bytes sit in a block buffer waiting to be flushed, they can interleave
  -- with whatever the children write in the meantime.
  hSetBuffering stderr LineBuffering
  -- Short-circuit: if the mutation manifest has no records at all (every
  -- module is disabled, e.g. via {-# ANN module ("DisableMutations" ...) #-},
  -- or the library has no instrumentable expressions), there is no work to
  -- do. Spawning one coverage child per leaf test would still run every
  -- test once — wasted setup cost when no mutation can possibly be covered.
  allRecords@(MutationManifest groups) <- mconcat <$> mapM readManifestDir manifestDirs
  augDir <- resolveAugmentedManifestDir (coverageParentAugmentedManifestDir covParent)
  let suiteName = fromMaybe "" (coverageParentSuiteName covParent)
      writeEmptyAugmented = do
        existing <- readAugmentedManifestFileIfExists augDir
        writeAugmentedManifestFile augDir (fromMaybe (AugmentedManifest []) existing)
  if all (\(MutationGroup rs) -> null rs) groups
    then do
      emitCoverageEvent settings (CoverageProgressSkipped CoverageSkipNoMutations)
      writeEmptyAugmented
    else do
      specForest <- execTestDefM settings spec
      let leafIds = map fst (flattenTestForestWithIds specForest)
          total = length leafIds
      if total == 0
        then do
          emitCoverageEvent settings (CoverageProgressSkipped CoverageSkipNoTests)
          writeEmptyAugmented
        else do
          defaultExe <- getExecutablePath
          n <- case coverageParentJobs covParent of
            Just j | j > 0 -> pure j
            _ -> getNumCapabilities
          sem <- newQSem n
          -- A coverage child that observes a test failure (and was launched
          -- with --mutation-fail-fast) throws 'CoverageFailFast' from its
          -- worker thread.  'mapConcurrently' cancels the remaining workers
          -- and re-raises the exception, which we catch here so we can exit
          -- non-zero rather than crashing with an unhandled exception.
          -- Mutation testing only makes sense against a passing baseline,
          -- so we do not write a partial augmented manifest.
          childResults <-
            Exception.handle (\CoverageFailFast -> hFlush stderr >> exitWith (ExitFailure 1)) $
              mapConcurrently
                (runCoverageChild defaultExe settings manifestDirs sem total)
                (zip [1 :: Int ..] leafIds)
          let (coverageMaps, baselineMaps) = unzip childResults
              TestCoverageMap coverageMap = mconcat coverageMaps
              TestBaselineMap baselineMap = mconcat baselineMaps
              mutationCoverage = invertCoverageMap coverageMap
          let newAugmented = buildAugmentedManifest suiteName mutationCoverage baselineMap allRecords
          existing <- readAugmentedManifestFileIfExists augDir
          let augmented = case existing of
                Nothing -> newAugmented
                Just prev -> mergeAugmentedManifests prev newAugmented
          writeAugmentedManifestFile augDir augmented
  where
    runCoverageChild defaultExe settings' manifestDirs' sem total (i, tid) =
      bracket_ (waitQSem sem) (signalQSem sem) $ do
        emitCoverageEvent settings' $
          CoverageProgressTest
            CoverageProgressTestEvent
              { coverageProgressIndex = i,
                coverageProgressTotal = total,
                coverageProgressTestId = tid,
                coverageProgressTestPhase = CoverageProgressStarting
              }
        result <- runCoverageChildAttempt defaultExe settings' manifestDirs' tid (coverageParentRetry covParent)
        let TestCoverageMap m = fst result
            covered = fromMaybe Set.empty (Map.lookup tid m)
        emitCoverageEvent settings' $
          CoverageProgressTest
            CoverageProgressTestEvent
              { coverageProgressIndex = i,
                coverageProgressTotal = total,
                coverageProgressTestId = tid,
                coverageProgressTestPhase = CoverageProgressDone (Set.size covered)
              }
        pure result

    -- Run one coverage child, retrying up to @retriesLeft@ times if it exits
    -- non-zero or writes an unreadable result.  The first attempt is one of
    -- the @retriesLeft + 1@ tries (i.e. @retriesLeft = 0@ means "no retries,
    -- one attempt").  Failure-after-all-retries is surfaced via 'fail'.
    runCoverageChildAttempt defaultExe settings' manifestDirs' tid retriesLeft = do
      result <- retryingIO retriesLeft (logCoverageRetry settings' tid) (runOneCoverageChild defaultExe settings' manifestDirs' tid)
      case result of
        Right v -> pure v
        Left reason ->
          fail $
            "coverage child for "
              ++ T.unpack (renderTestId tid)
              ++ ": "
              ++ reason

    -- One coverage-child attempt: returns @Left reason@ on transient
    -- failure (non-zero exit or unreadable output), @Right (cov, base)@
    -- on success.
    --
    -- Exit code 2 is reserved by 'runSingleCoverageMode' to mean "the test
    -- itself failed under fail-fast" — that is not a transient failure, so
    -- we throw 'CoverageFailFast' to abort the run without retrying.
    runOneCoverageChild defaultExe settings' manifestDirs' tid =
      withSystemTempDir "coverage-child" $ \tmpDir -> do
        let outputFile = fromAbsFile (tmpDir </> [relfile|coverage.json|])
            baselineFile = fromAbsFile (tmpDir </> [relfile|baseline.json|])
            -- Pass at least one --mutation-coverage dir so the child
            -- dispatches to runSingleCoverageMode rather than the normal
            -- test runner. The child ignores the manifest content; only
            -- the flag's presence matters for dispatch.
            coverageDirArgs = concatMap (\d -> ["--mutation-coverage", fromAbsDir d]) manifestDirs'
            failFastArg =
              if failFast
                then "--mutation-fail-fast"
                else "--no-mutation-fail-fast"
            args =
              coverageDirArgs
                ++ [ "--mutation-coverage-one",
                     T.unpack (renderTestId tid),
                     "--mutation-coverage-output",
                     outputFile,
                     "--mutation-coverage-baseline-output",
                     baselineFile,
                     failFastArg
                   ]
                ++ case coverageParentSuiteName covParent of
                  Nothing -> []
                  Just name -> ["--mutation-suite-name", T.unpack name]
            childProc = proc defaultExe args
        ec <- runProcess childProc
        case ec of
          ExitFailure 2 | failFast -> do
            hPutChunksLocaleWith (settingTerminalCapabilities settings') stderr $
              unlinesChunks
                [ [ fore red (chunk "coverage: test failed during baseline run for "),
                    chunk (renderTestId tid),
                    chunk " — aborting (fail-fast)"
                  ]
                ]
            Exception.throwIO CoverageFailFast
          ExitFailure code -> pure $ Left ("exited with code " ++ show code)
          ExitSuccess -> do
            mMap <- readTestCoverageMapFile outputFile
            case mMap of
              Nothing -> pure $ Left "wrote an unreadable coverage map"
              Just coverageMap -> do
                mBaseline <- readTestBaselineMapFile baselineFile
                case mBaseline of
                  Nothing -> pure $ Left "wrote an unreadable baseline map"
                  Just baselineMap -> pure $ Right (coverageMap, baselineMap)

    logCoverageRetry settings' tid reason retriesAfter =
      hPutChunksLocaleWith (settingTerminalCapabilities settings') stderr $
        unlinesChunks
          [ [ fore yellow (chunk "coverage: retrying "),
              chunk (renderTestId tid),
              chunk " (",
              chunk (T.pack reason),
              chunk ", ",
              chunk (T.pack (show retriesAfter)),
              chunk " retr",
              chunk (if retriesAfter == 1 then "y" else "ies"),
              chunk " left)"
            ]
          ]

    buildAugmentedManifest suiteName mutationCoverage baselineMap (MutationManifest groups) =
      AugmentedManifest
        [ AugmentedMutationGroup augmented
        | MutationGroup recs <- groups,
          let augmented = concatMap (annotateRecord suiteName mutationCoverage baselineMap) recs,
          not (null augmented)
        ]

    annotateRecord suiteName mutationCoverage baselineMap rec =
      let coveringTests =
            Set.toList $
              Map.findWithDefault Set.empty (mutRecId rec) mutationCoverage
          -- Per-mutation timeout (microseconds) = 10 * sum of covering-test
          -- baselines, floored at 30s.  Tests with no recorded baseline
          -- contribute 0; the floor still applies.
          coveringBaselineSum :: Word
          coveringBaselineSum =
            sum [Map.findWithDefault 0 t baselineMap | t <- coveringTests]
          timeoutMicros = max defaultTimeoutMicros (10 * coveringBaselineSum)
          annotated =
            rec
              { mutRecCoveringTests =
                  Just $ Map.singleton suiteName coveringTests
              }
       in case fromMutationRecord annotated of
            Nothing -> []
            Just r ->
              [r {augmentedMutationRecordTimeoutMicros = timeoutMicros}]

-- | Invert a @'Map' 'TestId' ('Set' 'MutationId')@ to
-- @'Map' 'MutationId' ('Set' 'TestId')@.
invertCoverageMap :: Map.Map TestId (Set.Set MutationId) -> Map.Map MutationId (Set.Set TestId)
invertCoverageMap =
  Map.foldlWithKey'
    ( \acc tid mids ->
        Set.foldl'
          (\a mid -> Map.insertWith Set.union mid (Set.singleton tid) a)
          acc
          mids
    )
    Map.empty

-- | Child process: run the single test identified by @--mutation-coverage-one@,
-- write its 'TestCoverageMap' to @--mutation-coverage-output@, write its
-- wall-clock baseline to @--mutation-coverage-baseline-output@, and exit.
runSingleCoverageMode :: Settings -> Bool -> CoverageChildSettings -> Spec -> IO ()
runSingleCoverageMode settings failFast covChild spec = do
  tid <- case parseTestIdFilterArg (coverageChildTestId covChild) of
    Nothing -> fail "runSingleCoverageMode: no valid coverage-child test id"
    Just t -> pure t
  let outputFile = coverageChildOutput covChild
      baselineFile = coverageChildBaselineOutput covChild
  specForest <- execTestDefM settings spec
  let coverageSettings =
        settings
          { settingThreads = Synchronous,
            settingMaxSuccess = 1
          }
      trie = testIdTrieFromList [tid]
      filtered = filterTestForestByTrie trie specForest
  ref <- newIORef Set.empty
  startTime <- getCurrentTime
  resultForest <- withCoverageSlot ref $ runSpecForestSynchronously coverageSettings filtered
  endTime <- getCurrentTime
  covered <- readIORef ref
  let coverageMap = TestCoverageMap (Map.singleton tid covered)
      elapsedMicros = diffUTCTimeMicros endTime startTime
  writeTestCoverageMapFile outputFile coverageMap
  writeTestBaselineMapFile baselineFile (TestBaselineMap (Map.singleton tid elapsedMicros))
  -- Mutation testing only makes sense against a passing baseline: if a test
  -- is red before any mutation is applied, its mutation scores are
  -- meaningless.  Print the offending test's output and a loud warning in
  -- both fail-fast and non-fail-fast cases.  Under fail-fast, also exit with
  -- code 2 so the parent aborts the run (see 'runCoverageMode').
  when (shouldExitFail settings (timedValue resultForest)) $ do
    printOutputSpecForest settings resultForest
    hPutChunksLocaleWith (settingTerminalCapabilities settings) stderr $
      unlinesChunks
        [ [ fore red $ chunk "coverage: WARNING: test failed during baseline run for ",
            fore red $ chunk (renderTestId tid),
            fore red $ chunk " — mutation scores against this baseline are unreliable"
          ]
        ]
    when failFast $ exitWith (ExitFailure 2)

-- | Convert a 'NominalDiffTime' difference to microseconds as a 'Word',
-- clamping negative results (clock skew) to zero.
diffUTCTimeMicros :: UTCTime -> UTCTime -> Word
diffUTCTimeMicros end start =
  let secs = realToFrac (diffUTCTime end start) :: Double
      micros = secs * 1000000
   in if micros <= 0 then 0 else ceiling micros

-- | Resolve the augmented manifest directory, falling back to the current
-- working directory when not specified.
resolveAugmentedManifestDir :: Maybe (Path Abs Dir) -> IO (Path Abs Dir)
resolveAugmentedManifestDir = maybe getCurrentDir pure

data MutationResult
  = MutationUncovered UncoveredMutation
  | MutationKilled AugmentedMutationRecord
  | -- | At least one suite's child exceeded its wall-clock timeout. The
    -- mutation is counted as killed in the overall score but also recorded
    -- separately in the report for visibility.
    MutationTimedOut TimedOutMutation
  | MutationSurvived SurvivedMutation
  | -- | The mutation was not tested because an earlier mutation in the same
    -- group already failed (survived or was uncovered).  Within-group
    -- fail-fast records every remaining alternative as 'MutationSkipped'
    -- without spawning a child.
    MutationSkipped SkippedMutation

-- | Per-suite outcome of running a single mutation child.
data SuiteOutcome
  = -- | Child exited non-zero — mutation killed by this suite.
    SuiteKilled
  | -- | Child exited zero — mutation survived in this suite. The optional
    -- log path points at the captured stdout/stderr (when a report dir is
    -- configured).
    SuiteSurvived (Maybe (Path Rel File))
  | -- | Child exceeded its wall-clock timeout and was terminated by the
    -- parent.  Carries the elapsed microseconds and (optionally) the
    -- log path.
    SuiteTimedOut Word (Maybe (Path Rel File))
  deriving (Eq, Show)

-- | Wrap a per-suite runner so that any synchronous exception escaping it is
-- treated as 'SuiteKilled'.  A mutation that makes the child process
-- unrunnable (e.g. 'BlockedIndefinitelyOnMVar' / @<<loop>>@ propagating from
-- 'waitExitCode', an 'IOException' from log-file IO, a parse failure on the
-- child's output) is conceptually indistinguishable from a mutation that
-- makes the child crash — both mean the test detected the mutation.
--
-- 'SomeAsyncException' is re-thrown so Ctrl-C and other cancellations still
-- propagate out of the runner.
classifySyncExceptionAsKilled :: IO SuiteOutcome -> IO SuiteOutcome
classifySyncExceptionAsKilled action =
  Exception.handle
    ( \(e :: Exception.SomeException) -> case Exception.fromException e of
        Just (_ :: Exception.SomeAsyncException) -> Exception.throwIO e
        Nothing -> pure SuiteKilled
    )
    action

-- | Retry an 'IO' action that produces a tagged failure reason.
--
-- @retryingIO retriesLeft onRetry action@ runs @action@; on 'Left' it calls
-- @onRetry@ with the reason and the number of retries remaining, then runs
-- @action@ again with a decremented counter.  When @retriesLeft@ reaches
-- zero, the final 'Left' is returned unchanged (so the caller can decide
-- how to surface the failure).
--
-- A @retriesLeft@ of 0 means "no retries, one attempt".  A @retriesLeft@ of
-- 3 means "up to 4 total attempts".
--
-- The action itself decides what counts as a transient failure: returning
-- 'Right' bypasses retry, returning 'Left' triggers it.
--
-- Synchronous exceptions thrown by @action@ are not caught here — they are
-- not, in the coverage-child use case, the kind of failure we want to
-- retry through.  Use 'Exception.handle' inside @action@ if you need to
-- convert exceptions to 'Left'.
retryingIO ::
  -- | Initial number of retries (0 = no retries; one attempt total).
  Word ->
  -- | Called once per retry, with the reason and the number of retries
  -- still remaining after the current attempt.
  (String -> Word -> IO ()) ->
  -- | The action to run.
  IO (Either String a) ->
  IO (Either String a)
retryingIO retriesLeft onRetry action = do
  result <- action
  case result of
    Right v -> pure (Right v)
    Left reason
      | retriesLeft > 0 -> do
          onRetry reason (retriesLeft - 1)
          retryingIO (retriesLeft - 1) onRetry action
      | otherwise -> pure (Left reason)

-- | Parent process: read @manifest-augmented.json@ and spawn one child
-- subprocess per mutation per suite that covers it.
--
-- Each child receives @--mutation <dir> --mutation-one <id>
-- --mutation-suite-name <suite> --mutation-augmented-manifest-dir <dir>@
-- and exits 0 (survived) or non-zero (killed).
--
-- Suites with no covering tests for a given mutation are skipped — running a
-- suite with an empty filter would cause sydtest to run all tests.
--
-- Prints @Killed: N@, @Survived: M@, and @Uncovered: K@ so the Nix report
-- derivation can parse them.  Also writes @report.json@ to
-- 'settingMutationReportDir' when set.
-- | Thrown inside a 'mapConcurrently' worker to abort the mutation run when
-- fail-fast is on and a surviving or uncovered mutation is observed.  The
-- sibling workers are cancelled by 'mapConcurrently' on the first exception.
data MutationFailFast = MutationFailFast
  deriving (Show)

instance Exception MutationFailFast

-- | Thrown inside a coverage 'mapConcurrently' worker to abort the run when
-- a coverage child reports that its baseline test failed (exit code 2) and
-- fail-fast is on.  Distinct from 'MutationFailFast' because the cause is
-- different — the suite was already red before any mutation ran, so the
-- mutation scores would be meaningless.
data CoverageFailFast = CoverageFailFast
  deriving (Show)

instance Exception CoverageFailFast

-- | A mutation result that should trip within-group fail-fast: the test
-- suite did not detect the mutation (survivor) or no test reaches the
-- mutation site (uncovered).  Timeouts count as killed, so they do not
-- trip; 'MutationSkipped' is itself a consequence of a prior failure and
-- does not trip again.
isMutationFailure :: MutationResult -> Bool
isMutationFailure = \case
  MutationSurvived _ -> True
  MutationUncovered _ -> True
  MutationKilled _ -> False
  MutationTimedOut _ -> False
  MutationSkipped _ -> False

-- | The 'MutationId' of the mutation that produced a failing result, used as
-- the @cause@ on 'SkippedMutation' entries for subsequent group members.
mutationResultId :: MutationResult -> Maybe MutationId
mutationResultId = \case
  MutationSurvived sm -> Just (augmentedMutationRecordId (survivedMutationRecord sm))
  MutationUncovered um -> Just (augmentedMutationRecordId (uncoveredMutationRecord um))
  _ -> Nothing

resultToOutcome :: MutationResult -> MutationOutcome
resultToOutcome = \case
  MutationKilled r -> OutcomeKilled r
  MutationSurvived sm -> OutcomeSurvived sm
  MutationTimedOut tm -> OutcomeTimedOut tm
  MutationUncovered um -> OutcomeUncovered um
  MutationSkipped sk -> OutcomeSkipped sk

-- | Per-outcome-kind counts produced by 'tallyGroups'.  Each 'OutcomeTimedOut'
-- bumps both 'tallyKilled' and 'tallyTimedOut' because a timed-out mutation is
-- treated as killed for scoring; 'tallyTimedOut' carries the separate count
-- for visibility.
data OutcomeTally = OutcomeTally
  { tallyKilled :: !Word,
    tallySurvived :: !Word,
    tallyTimedOut :: !Word,
    tallyUncovered :: !Word,
    tallySkipped :: !Word
  }

emptyOutcomeTally :: OutcomeTally
emptyOutcomeTally =
  OutcomeTally
    { tallyKilled = 0,
      tallySurvived = 0,
      tallyTimedOut = 0,
      tallyUncovered = 0,
      tallySkipped = 0
    }

-- | Run one mutation group: walk records sequentially, calling the supplied
-- @run@ for each record, and once one record's result is 'isMutationFailure'
-- record every remaining record as 'MutationSkipped' instead of running it.
--
-- Returns the results in source order.  When @globalFailFast@ is 'True' and
-- a non-skipped result is a failure, 'MutationFailFast' is thrown after the
-- group's results have been written to the accumulator so a partial report
-- can still be assembled by the caller.
runOneGroup ::
  -- | Whether to throw 'MutationFailFast' after the first failing result.
  Bool ->
  -- | How to run one mutation.  Receives the record being tested.
  (AugmentedMutationRecord -> IO MutationResult) ->
  -- | Called once per produced 'MutationResult' (in source order) so the
  -- caller can stream results into an accumulator.
  (MutationResult -> IO ()) ->
  -- | Records of this group in source order.
  [AugmentedMutationRecord] ->
  IO ()
runOneGroup globalFailFast runOne onResult = loop Nothing
  where
    loop _ [] = pure ()
    loop (Just causeMid) (rec : rest) = do
      let r = MutationSkipped (SkippedMutation rec causeMid)
      onResult r
      loop (Just causeMid) rest
    loop Nothing (rec : rest) = do
      r <- runOne rec
      onResult r
      when (globalFailFast && isMutationFailure r) $
        Exception.throwIO MutationFailFast
      loop (mutationResultId r) rest

runMutationMode :: Settings -> Bool -> MutationParentSettings -> Spec -> IO ()
runMutationMode settings failFast mutParent _spec = do
  hSetBuffering stderr (BlockBuffering Nothing)
  augDir <- resolveAugmentedManifestDir (mutationParentAugmentedManifestDir mutParent)
  AugmentedManifest groups <- readAugmentedManifestFile augDir
  defaultExe <- getExecutablePath
  n <- getNumCapabilities
  sem <- newQSem n
  -- Each group's per-mutation results, accumulated in source order so a
  -- partial report (after a global fail-fast abort) reflects the work done.
  groupResultsVar <- newTVarIO (Map.empty :: Map.Map Int [MutationResult])
  let runGroup' (gix, AugmentedMutationGroup recs) =
        runOneGroup
          failFast
          (runOne defaultExe augDir sem)
          ( \r ->
              atomically $
                modifyTVar' groupResultsVar (Map.insertWith (++) gix [r])
          )
          recs
  Exception.handle (\MutationFailFast -> pure ()) $ do
    _ <- mapConcurrently runGroup' (zip [0 :: Int ..] groups)
    pure ()
  finalGroupResults <- readTVarIO groupResultsVar
  -- Preserve original group order; reverse each group's results because they
  -- were accumulated cons-style.
  let groupReports =
        [ MutationGroupReport (map resultToOutcome (reverse (Map.findWithDefault [] gix finalGroupResults)))
        | gix <- [0 .. length groups - 1]
        ]
      OutcomeTally
        { tallyKilled = killed,
          tallySurvived = survived,
          tallyTimedOut = timedOut,
          tallyUncovered = uncovered,
          tallySkipped = skipped
        } = tallyGroups groupReports
      jsonReport =
        MutationRunReport
          { mutationRunReportKilled = killed,
            mutationRunReportSurvived = survived,
            mutationRunReportTimedOut = timedOut,
            mutationRunReportUncovered = uncovered,
            mutationRunReportSkipped = skipped,
            mutationRunReportGroups = groupReports
          }
  mapM_ (`writeMutationRunReport` jsonReport) (mutationParentReportDir mutParent)
  putChunksLocaleWith (settingTerminalCapabilities settings) (unlinesChunks (renderMutationRunReport jsonReport))
  -- Force out any block-buffered progress events before we either return
  -- normally or exit non-zero. Without this, the last few lines of
  -- progress can be dropped when 'exitWith' tears down the runtime.
  hFlush stderr
  when (failFast && (survived > 0 || uncovered > 0)) $ exitWith (ExitFailure 1)
  where
    tallyGroups :: [MutationGroupReport] -> OutcomeTally
    tallyGroups = foldr (\(MutationGroupReport os) acc -> foldr step acc os) emptyOutcomeTally
      where
        step = \case
          OutcomeKilled _ -> \t -> t {tallyKilled = tallyKilled t + 1}
          OutcomeTimedOut _ -> \t -> t {tallyKilled = tallyKilled t + 1, tallyTimedOut = tallyTimedOut t + 1}
          OutcomeSurvived _ -> \t -> t {tallySurvived = tallySurvived t + 1}
          OutcomeUncovered _ -> \t -> t {tallyUncovered = tallyUncovered t + 1}
          OutcomeSkipped _ -> \t -> t {tallySkipped = tallySkipped t + 1}

    runOne defaultExe augDir sem record =
      bracket_ (waitQSem sem) (signalQSem sem) $ do
        let mid = augmentedMutationRecordId record
        hPutChunksLocaleWith (settingTerminalCapabilities settings) stderr (unlinesChunks (renderMutationProgressEvent (MutationProgressEvent record)))
        -- Only run suites that have at least one covering test for this mutation.
        let coveringBySuite =
              Map.filter (not . null) (augmentedMutationRecordCoveringTests record)
        case NE.nonEmpty (Map.keys coveringBySuite) of
          Nothing -> pure (MutationUncovered (UncoveredMutation record))
          Just suiteNames -> do
            -- Run one child per covering suite. The mutation is killed if any
            -- child exits non-zero; timed out (counted as killed) if any
            -- child exceeded its budget without any other child killing it
            -- first; otherwise survived.
            outcomes <- mapM (runOneSuite defaultExe augDir record mid) suiteNames
            pure $ classifyOutcomes record outcomes

    classifyOutcomes record outcomes
      | any isKilled outcomes = MutationKilled record
      | otherwise = case mTimedOut of
          Just (elapsedMicros, mLog) ->
            MutationTimedOut
              TimedOutMutation
                { timedOutMutationRecord = record,
                  timedOutMutationElapsedMicros = elapsedMicros,
                  timedOutMutationLogFile = mLog
                }
          Nothing ->
            MutationSurvived
              SurvivedMutation
                { survivedMutationRecord = record,
                  -- Prefer a survivor suite that produced a log file; fall back
                  -- to no log otherwise.  By non-emptiness of @outcomes@ and the
                  -- branches above, at least one element is 'SuiteSurvived'.
                  survivedMutationLogFile =
                    listToMaybe [rf | SuiteSurvived (Just rf) <- NE.toList outcomes]
                }
      where
        isKilled SuiteKilled = True
        isKilled _ = False
        mTimedOut = listToMaybe [(micros, mLog) | SuiteTimedOut micros mLog <- NE.toList outcomes]

    runOneSuite defaultExe augDir record mid suiteName = do
      let suiteExes = mutationParentSuiteExes mutParent
          exe = fromMaybe defaultExe (Map.lookup suiteName suiteExes)
          rtsArgs = case mutationParentChildMemLimit mutParent of
            Nothing -> []
            Just limit -> ["+RTS", "-M" ++ limit, "-RTS"]
          suiteNameStr = T.unpack suiteName
          args =
            [ "--mutation",
              fromAbsDir augDir,
              "--mutation-one",
              renderMutationId mid,
              "--mutation-augmented-manifest-dir",
              fromAbsDir augDir,
              "--mutation-suite-name",
              suiteNameStr
            ]
              ++ rtsArgs
      classifySyncExceptionAsKilled $
        withSystemTempDir "mutation-child" $ \tmpDir -> do
          let logPath = tmpDir </> [relfile|child.log|]
          (outcomeRaw, elapsedMicros) <-
            withFile (fromAbsFile logPath) WriteMode $ \logHandle -> do
              let childProc =
                    setStdout (useHandleOpen logHandle) $
                      setStderr (useHandleOpen logHandle) $
                        proc exe args
                  -- Per-mutation wall-clock budget computed by the coverage phase.
                  timeoutMicros = augmentedMutationRecordTimeoutMicros record
                  -- Cap the threadDelay argument at maxBound Int so very large
                  -- budgets don't overflow when converted to the Int that
                  -- threadDelay expects.
                  micros =
                    if timeoutMicros >= fromIntegral (maxBound :: Int)
                      then maxBound :: Int
                      else fromIntegral timeoutMicros
              startTime <- getCurrentTime
              raw <- startProcessAndWait childProc micros
              endTime <- getCurrentTime
              pure (raw, diffUTCTimeMicros endTime startTime)
          case outcomeRaw of
            Left () -> do
              -- Timed out: parent killed the child. Preserve whatever the
              -- child managed to write so the report retains useful context.
              mRelFile <- copyChildLog "timeout-" mid suiteName logPath
              pure (SuiteTimedOut elapsedMicros mRelFile)
            Right ec -> case ec of
              ExitFailure _ -> pure SuiteKilled
              ExitSuccess -> do
                mRelFile <- copyChildLog "survivor-" mid suiteName logPath
                pure (SuiteSurvived mRelFile)

    copyChildLog prefix mid suiteName logPath =
      case mutationParentReportDir mutParent of
        Nothing -> pure Nothing
        Just reportDir -> do
          let suiteNameStr = T.unpack suiteName
              logName =
                prefix
                  ++ map (\c -> if c == '/' then '-' else c) (renderMutationId mid)
                  ++ ( if T.null suiteName
                         then ""
                         else "-" ++ suiteNameStr
                     )
                  ++ ".log"
          case parseRelFile logName of
            Nothing -> pure Nothing
            Just relFile -> do
              copyFile logPath (reportDir </> relFile)
              pure (Just relFile)

    -- Race the child against a delay; on timeout, stop the process (SIGTERM
    -- via System.Process.Typed's stopProcess; SIGKILL follows after the
    -- library's grace period) and report a Left (timeout) outcome.
    --
    -- bracket guarantees the child is reaped on both the timeout-wins branch
    -- and on async exceptions propagating into this thread.  When the inner
    -- branch wins, the child has already been reaped by 'waitExitCode'; the
    -- cleanup's 'stopProcess' then calls 'waitForProcess' on an already-reaped
    -- pid and throws ECHILD.  'ignoringAbsence' silences exactly that
    -- not-found case and rethrows anything else.
    startProcessAndWait childProc micros =
      bracket (startProcess childProc) (ignoringAbsence . stopProcess) $ \p -> do
        result <- race (threadDelay micros) (waitExitCode p)
        case result of
          Left () -> pure (Left ())
          Right ec -> pure (Right ec)

renderMutationRunReport :: MutationRunReport -> [[Chunk]]
renderMutationRunReport
  MutationRunReport
    { mutationRunReportKilled,
      mutationRunReportSurvived,
      mutationRunReportTimedOut,
      mutationRunReportUncovered,
      mutationRunReportSkipped,
      mutationRunReportGroups
    } =
    [ [chunk "Killed: ", fore green (chunk (T.pack (show mutationRunReportKilled)))],
      [chunk "  (of which timed out: ", fore yellow (chunk (T.pack (show mutationRunReportTimedOut))), chunk ")"],
      [chunk "Survived: ", fore red (chunk (T.pack (show mutationRunReportSurvived)))],
      [chunk "Uncovered: ", fore yellow (chunk (T.pack (show mutationRunReportUncovered)))],
      [chunk "Skipped: ", fore yellow (chunk (T.pack (show mutationRunReportSkipped)))]
    ]
      ++ ( if null timedOuts
             then []
             else
               [[], [chunk "Timed-out mutations:"]]
                 ++ concatMap renderTimedOut timedOuts
         )
      ++ ( if null survivors
             then []
             else
               [[], [chunk "Surviving mutations:"]]
                 ++ concatMap renderSurvivor survivors
                 ++ [[], remediationHeader "To resolve a surviving mutation:"]
                 ++ remediationSurvivorBody
         )
      ++ ( if null uncovereds
             then []
             else
               [[], [chunk "Uncovered mutations:"]]
                 ++ concatMap renderUncovered uncovereds
                 ++ [[], remediationHeader "To resolve an uncovered mutation:"]
                 ++ remediationUncoveredBody
         )
      ++ ( if null skippeds
             then []
             else
               [[], [chunk "Skipped mutations:"]]
                 ++ concatMap renderSkipped skippeds
         )
    where
      allOutcomes = concatMap mutationGroupReportOutcomes mutationRunReportGroups
      survivors = [s | OutcomeSurvived s <- allOutcomes]
      timedOuts = [t | OutcomeTimedOut t <- allOutcomes]
      uncovereds = [u | OutcomeUncovered u <- allOutcomes]
      skippeds = [sk | OutcomeSkipped sk <- allOutcomes]
      renderSurvivor sm =
        let rec = survivedMutationRecord sm
            mid = augmentedMutationRecordId rec
         in [] : formatMutationLog mid rec
      renderTimedOut tm =
        let rec = timedOutMutationRecord tm
            mid = augmentedMutationRecordId rec
            secs = fromIntegral (timedOutMutationElapsedMicros tm) / (1000000 :: Double)
            header =
              [ chunk "[timed out after ",
                fore yellow (chunk (T.pack (show secs))),
                chunk "s]"
              ]
         in [] : header : formatMutationLog mid rec
      renderUncovered um =
        let rec = uncoveredMutationRecord um
            mid = augmentedMutationRecordId rec
         in [] : formatMutationLog mid rec
      renderSkipped sk =
        let rec = skippedMutationRecord sk
            mid = augmentedMutationRecordId rec
            cause = skippedMutationCause sk
            header =
              [ chunk "[skipped — failed in same group: ",
                fore yellow (chunk (T.pack (renderMutationId cause))),
                chunk "]"
              ]
         in [] : header : formatMutationLog mid rec
      remediationHeader t = [fore cyan (chunk t)]
      -- Kept in sync with the disable-annotation syntax in
      -- sydtest-mutation-plugin (Test.Syd.Mutation.Plugin.Instrument:
      -- 'parseFunMutationAnns') and the global
      -- 'mutationPluginConfigDisabledMutations' / 'exceptions' fields in
      -- Test.Syd.Mutation.Plugin.OptParse.
      remediationSurvivorBody =
        [ [chunk "  1. Kill it: add or strengthen a test so the mutation causes a test failure."],
          [chunk "  2. Disable it on this binding:"],
          [chunk "       {-# ANN funName (\"DisableMutation: <Operator>\" :: String) #-}"],
          [chunk "     or for every operator on a binding:"],
          [chunk "       {-# ANN funName (\"DisableMutations\" :: String) #-}"],
          [chunk "     or for the whole module:"],
          [chunk "       {-# ANN module (\"DisableMutations\" :: String) #-}"],
          [chunk "     or globally in the plugin config (sydtest-mutation-plugin.yaml):"],
          [chunk "       disabled-mutations: [<Operator>]"]
        ]
      remediationUncoveredBody =
        [ [chunk "  1. Cover it: add a test that exercises the mutation site so the coverage phase records a covering test."],
          [chunk "  2. Disable it: same annotations and config keys as for survivors above."]
        ]

renderMutationProgressEvent :: MutationProgressEvent -> [[Chunk]]
renderMutationProgressEvent (MutationProgressEvent rec) =
  let logLines = formatMutationLog (augmentedMutationRecordId rec) rec
   in case logLines of
        [] -> [[chunk "Testing mutation"]]
        (firstLine : rest) -> (chunk "Testing mutation " : firstLine) : rest

-- | Progress event for the coverage phase.
data CoverageProgressEvent
  = -- | Per-test event: emitted once when a coverage child for a test is
    -- about to run, and once when it has finished.
    CoverageProgressTest !CoverageProgressTestEvent
  | -- | Suite-level event: emitted once when the whole coverage phase is
    -- skipped because there is nothing to do.
    CoverageProgressSkipped !CoverageProgressSkipReason

data CoverageProgressTestEvent = CoverageProgressTestEvent
  { coverageProgressIndex :: !Int,
    coverageProgressTotal :: !Int,
    coverageProgressTestId :: !TestId,
    coverageProgressTestPhase :: !CoverageProgressPhase
  }

data CoverageProgressPhase
  = CoverageProgressStarting
  | -- | Number of mutations covered by this test.
    CoverageProgressDone !Int

-- | Why the coverage phase was skipped.
data CoverageProgressSkipReason
  = -- | The mutation manifest contained no records (every instrumentable
    -- module was disabled, e.g. via @{-# ANN module ("DisableMutations" ...) #-}@).
    CoverageSkipNoMutations
  | -- | The test spec produced no leaf tests, so there is nothing to run
    -- coverage on.
    CoverageSkipNoTests

renderCoverageProgressEvent :: CoverageProgressEvent -> [[Chunk]]
renderCoverageProgressEvent = \case
  CoverageProgressTest CoverageProgressTestEvent {coverageProgressIndex, coverageProgressTotal, coverageProgressTestId, coverageProgressTestPhase} ->
    let prefix =
          [ chunk "coverage (",
            chunk (T.pack (show coverageProgressIndex)),
            chunk "/",
            chunk (T.pack (show coverageProgressTotal)),
            chunk "): "
          ]
        tidChunk = chunk (renderTestId coverageProgressTestId)
     in case coverageProgressTestPhase of
          CoverageProgressStarting ->
            [prefix ++ [fore cyan (chunk "running "), tidChunk]]
          CoverageProgressDone n ->
            [ prefix
                ++ [ fore green (chunk "done "),
                     tidChunk,
                     chunk " (",
                     chunk (T.pack (show n)),
                     chunk " mutations)"
                   ]
            ]
  CoverageProgressSkipped reason ->
    [ [ fore yellow (chunk "coverage: skipped "),
        chunk $ case reason of
          CoverageSkipNoMutations -> "(no mutations in manifest)"
          CoverageSkipNoTests -> "(no tests in spec)"
      ]
    ]

emitCoverageEvent :: Settings -> CoverageProgressEvent -> IO ()
emitCoverageEvent settings ev =
  hPutChunksLocaleWith
    (settingTerminalCapabilities settings)
    stderr
    (unlinesChunks (renderCoverageProgressEvent ev))

formatMutationLog :: MutationId -> AugmentedMutationRecord -> [[Chunk]]
formatMutationLog (MutationId parts) AugmentedMutationRecord {augmentedMutationRecordOperator, augmentedMutationRecordOriginal, augmentedMutationRecordReplacement, augmentedMutationRecordSourceLines, augmentedMutationRecordMutatedLines, augmentedMutationRecordSourceFile, augmentedMutationRecordLine, augmentedMutationRecordContextBefore, augmentedMutationRecordContextAfter} =
  case parts of
    (modName : _op : lineStr : colStartStr : colEndStr : _) ->
      let filePath = case augmentedMutationRecordSourceFile of
            Just p -> fromRelFile p
            Nothing -> moduleToFilePath modName
          -- Append "#<index>" so identical replStr alternatives (e.g. ListLit's
          -- drop-first and drop-last on a 3-element list, both "2 elements")
          -- are distinguishable in the human-readable header line.
          variantSuffix = case parts of
            [_, _, _, _, _, _, altIdx] -> " #" ++ altIdx
            _ -> ""
          headerText = T.pack $ T.unpack augmentedMutationRecordOperator ++ " at " ++ filePath ++ ":" ++ lineStr ++ ":" ++ colStartStr ++ "-" ++ colEndStr ++ variantSuffix
          headerLine = [chunk headerText]
       in case augmentedMutationRecordSourceLines of
            [] ->
              [ headerLine,
                [fore red (chunk ("    - " <> augmentedMutationRecordOriginal))],
                [fore green (chunk ("    + " <> augmentedMutationRecordReplacement))]
              ]
            _ ->
              headerLine : renderUnifiedDiff (fromIntegral augmentedMutationRecordLine) augmentedMutationRecordContextBefore augmentedMutationRecordSourceLines augmentedMutationRecordMutatedLines augmentedMutationRecordContextAfter
    _ ->
      [[chunk (T.pack $ intercalate "/" parts)]]
  where
    moduleToFilePath m = map (\c -> if c == '.' then '/' else c) m ++ ".hs"

renderUnifiedDiff :: Int -> [Text] -> [Text] -> [Text] -> [Text] -> [[Chunk]]
renderUnifiedDiff startLine ctxBefore srcLines mutLines ctxAfter =
  let allBefore = ctxBefore ++ srcLines ++ ctxAfter
      allAfter = ctxBefore ++ mutLines ++ ctxAfter
      groups = getGroupedDiff allBefore allAfter
      hunkStart = startLine - length ctxBefore
      origCount = length allBefore
      mutCount = length allAfter
      hunkHeader =
        T.pack $
          "@@ -"
            ++ show hunkStart
            ++ ","
            ++ show origCount
            ++ " +"
            ++ show hunkStart
            ++ ","
            ++ show mutCount
            ++ " @@"
   in [fore cyan (chunk hunkHeader)] : renderGroups groups
  where
    renderGroups :: [PolyDiff [Text] [Text]] -> [[Chunk]]
    renderGroups [] = []
    renderGroups (Both ls _ : rest) =
      map (\l -> [chunk (T.cons ' ' l)]) ls ++ renderGroups rest
    renderGroups gs@(First _ : _) =
      let (dels, adds, rest) = collectChange gs
       in renderPaired dels adds ++ renderGroups rest
    renderGroups gs@(Second _ : _) =
      let (dels, adds, rest) = collectChange gs
       in renderPaired dels adds ++ renderGroups rest

    collectChange :: [PolyDiff [Text] [Text]] -> ([Text], [Text], [PolyDiff [Text] [Text]])
    collectChange = go [] []
      where
        go ds as (First ls : rest) = go (ds ++ ls) as rest
        go ds as (Second ls : rest) = go ds (as ++ ls) rest
        go ds as rest = (ds, as, rest)

    renderDelLine :: Text -> [Chunk]
    renderDelLine l = [fore delColour (chunk (T.cons '-' l))]

    renderAddLine :: Text -> [Chunk]
    renderAddLine l = [fore addColour (chunk (T.cons '+' l))]

    renderPaired :: [Text] -> [Text] -> [[Chunk]]
    renderPaired dels adds =
      let n = min (length dels) (length adds)
          (pairedDels, extraDels) = splitAt n dels
          (pairedAdds, extraAdds) = splitAt n adds
          (delLines, addLines) = unzip (zipWith renderPair pairedDels pairedAdds)
       in delLines ++ map renderDelLine extraDels ++ addLines ++ map renderAddLine extraAdds

    renderPair :: Text -> Text -> ([Chunk], [Chunk])
    renderPair delLine addLine =
      let charDiff = V.toList (getTextDiff delLine addLine)
          delChunks = fore delColour (chunk (T.singleton '-')) : renderDelSide charDiff
          addChunks = fore addColour (chunk (T.singleton '+')) : renderAddSide charDiff
       in (delChunks, addChunks)
