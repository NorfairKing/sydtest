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
    runSingleMutationMode,
    runCoverageMode,
    runSingleCoverageMode,
    renderMutationRunReport,
    renderMutationProgressEvent,
    CoverageProgressEvent (..),
    CoverageProgressTestEvent (..),
    CoverageProgressPhase (..),
    CoverageProgressSkipReason (..),
    renderCoverageProgressEvent,
    formatMutationLog,
    renderMutationAddedEvent,
    renderUnifiedDiff,

    -- * Exposed for testing
    SuiteOutcome (..),
    classifySyncExceptionAsKilled,
    retryingIO,
  )
where

import Control.Concurrent (newQSem, signalQSem, threadDelay, waitQSem)
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Exception (IOException, bracket, bracket_)
import qualified Control.Exception as Exception
import Data.IORef
import Data.List (intercalate)
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
import Path.IO (copyFile, getCurrentDir, withSystemTempDir)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (BufferMode (..), IOMode (..), hClose, hSetBuffering, openFile, stderr)
import System.Process.Typed (proc, runProcess, setStderr, setStdout, startProcess, stopProcess, useHandleOpen, waitExitCode)
import Test.Syd.Def
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationRecord (..),
    MutationProgressEvent (..),
    MutationRunReport (..),
    SurvivedMutation (..),
    TimedOutMutation (..),
    UncoveredMutation (..),
    fromMutationRecord,
    lookupAugmentedMutationRecord,
    mergeAugmentedManifests,
    readAugmentedManifestFile,
    readAugmentedManifestFileIfExists,
    writeAugmentedManifestFile,
    writeMutationRunReport,
  )
import Test.Syd.Mutation.Forest (filterTestForestByTrie, flattenTestForestWithIds, testIdTrieFromList)
import Test.Syd.Mutation.Manifest
  ( MutationAddedEvent (..),
    MutationManifest (..),
    MutationRecord (..),
    readManifestDir,
  )
import Test.Syd.Mutation.Runtime (MutationId (..), parseMutationId, renderMutationId, setActiveMutation, withCoverageSlot)
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
-- @manifest-augmented.json@ in @settingMutationAugmentedManifestDir@.
--
-- When @settingMutationSuiteName@ is set, covering tests are recorded under
-- that suite name key.  If @manifest-augmented.json@ already exists (from a
-- prior suite's coverage pass), this run's results are merged in so that
-- multiple suites can be run sequentially.
runCoverageMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runCoverageMode settings manifestDirs spec = do
  -- Short-circuit: if the mutation manifest has no records at all (every
  -- module is disabled, e.g. via {-# ANN module ("DisableMutations" ...) #-},
  -- or the library has no instrumentable expressions), there is no work to
  -- do. Spawning one coverage child per leaf test would still run every
  -- test once — wasted setup cost when no mutation can possibly be covered.
  allRecords@(MutationManifest records) <- mconcat <$> mapM readManifestDir manifestDirs
  augDir <- resolveAugmentedManifestDir settings
  let suiteName = fromMaybe "" (settingMutationSuiteName settings)
      writeEmptyAugmented = do
        existing <- readAugmentedManifestFileIfExists augDir
        writeAugmentedManifestFile augDir (fromMaybe (AugmentedManifest []) existing)
  if null records
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
          n <- case settingMutationCoverageJobs settings of
            Just j | j > 0 -> pure j
            _ -> getNumCapabilities
          sem <- newQSem n
          childResults <-
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
        result <- runCoverageChildAttempt defaultExe settings' manifestDirs' tid (settingMutationCoverageRetry settings')
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
    runOneCoverageChild defaultExe settings' manifestDirs' tid =
      withSystemTempDir "coverage-child" $ \tmpDir -> do
        let outputFile = fromAbsFile (tmpDir </> [relfile|coverage.json|])
            baselineFile = fromAbsFile (tmpDir </> [relfile|baseline.json|])
            -- Pass at least one --mutation-coverage dir so the child
            -- dispatches to runSingleCoverageMode rather than the normal
            -- test runner. The child ignores the manifest content; only
            -- the flag's presence matters for dispatch.
            coverageDirArgs = concatMap (\d -> ["--mutation-coverage", fromAbsDir d]) manifestDirs'
            args =
              coverageDirArgs
                ++ [ "--mutation-coverage-one",
                     T.unpack (renderTestId tid),
                     "--mutation-coverage-output",
                     outputFile,
                     "--mutation-coverage-baseline-output",
                     baselineFile
                   ]
                ++ case settingMutationSuiteName settings' of
                  Nothing -> []
                  Just name -> ["--mutation-suite-name", T.unpack name]
            childProc = proc defaultExe args
        ec <- runProcess childProc
        case ec of
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

    buildAugmentedManifest suiteName mutationCoverage baselineMap (MutationManifest records) =
      AugmentedManifest $
        concatMap (annotateRecord suiteName mutationCoverage baselineMap) records

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
          timeoutMicros = max 30000000 (10 * coveringBaselineSum)
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
runSingleCoverageMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runSingleCoverageMode settings _manifestDirs spec = do
  tid <- case settingMutationCoverageOne settings >>= parseTestIdFilterArg of
    Nothing -> fail "runSingleCoverageMode: no valid --mutation-coverage-one id"
    Just t -> pure t
  outputFile <- case settingMutationCoverageOutput settings of
    Nothing -> fail "runSingleCoverageMode: no --mutation-coverage-output file"
    Just f -> pure f
  baselineFile <- case settingMutationCoverageBaselineOutput settings of
    Nothing -> fail "runSingleCoverageMode: no --mutation-coverage-baseline-output file"
    Just f -> pure f
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
  _ <- withCoverageSlot ref $ runSpecForestSynchronously coverageSettings filtered
  endTime <- getCurrentTime
  covered <- readIORef ref
  let coverageMap = TestCoverageMap (Map.singleton tid covered)
      elapsedMicros = diffUTCTimeMicros endTime startTime
  writeTestCoverageMapFile outputFile coverageMap
  writeTestBaselineMapFile baselineFile (TestBaselineMap (Map.singleton tid elapsedMicros))

-- | Convert a 'NominalDiffTime' difference to microseconds as a 'Word',
-- clamping negative results (clock skew) to zero.
diffUTCTimeMicros :: UTCTime -> UTCTime -> Word
diffUTCTimeMicros end start =
  let secs = realToFrac (diffUTCTime end start) :: Double
      micros = secs * 1000000
   in if micros <= 0 then 0 else ceiling micros

-- | Resolve the augmented manifest directory from settings,
-- falling back to the current working directory.
resolveAugmentedManifestDir :: Settings -> IO (Path Abs Dir)
resolveAugmentedManifestDir settings =
  maybe getCurrentDir pure (settingMutationAugmentedManifestDir settings)

data MutationResult
  = MutationUncovered UncoveredMutation
  | MutationKilled
  | -- | At least one suite's child exceeded its wall-clock timeout. The
    -- mutation is counted as killed in the overall score but also recorded
    -- in 'mutationRunReportTimedOut' / 'mutationRunReportTimedOutMutations'
    -- for visibility.
    MutationTimedOut (Maybe TimedOutMutation)
  | MutationSurvived (Maybe SurvivedMutation)

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
runMutationMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runMutationMode settings _manifestDirs _spec = do
  hSetBuffering stderr (BlockBuffering Nothing)
  augDir <- resolveAugmentedManifestDir settings
  AugmentedManifest records <- readAugmentedManifestFile augDir
  defaultExe <- getExecutablePath
  n <- getNumCapabilities
  sem <- newQSem n
  results <- mapConcurrently (runOne defaultExe augDir sem) records
  let (killed, survived, timedOut, survivors, timedOuts, uncoveredMutations) =
        foldr tally (0 :: Word, 0, 0, [], [], []) results
      uncovered = fromIntegral (length uncoveredMutations) :: Word
      jsonReport =
        MutationRunReport
          { mutationRunReportKilled = killed,
            mutationRunReportSurvived = survived,
            mutationRunReportTimedOut = timedOut,
            mutationRunReportUncovered = uncovered,
            mutationRunReportSurvivors = survivors,
            mutationRunReportTimedOutMutations = timedOuts,
            mutationRunReportUncoveredMutations = uncoveredMutations
          }
  mapM_ (`writeMutationRunReport` jsonReport) (settingMutationReportDir settings)
  putChunksLocaleWith (settingTerminalCapabilities settings) (unlinesChunks (renderMutationRunReport jsonReport))
  where
    -- Timed-out mutations are counted as killed; the timedOut count and
    -- list are reported separately for visibility.
    tally (MutationUncovered um) (k, s, t, ss, ts, us) = (k, s, t, ss, ts, us ++ [um])
    tally MutationKilled (k, s, t, ss, ts, us) = (k + 1, s, t, ss, ts, us)
    tally (MutationTimedOut Nothing) (k, s, t, ss, ts, us) = (k + 1, s, t + 1, ss, ts, us)
    tally (MutationTimedOut (Just tm)) (k, s, t, ss, ts, us) = (k + 1, s, t + 1, ss, ts ++ [tm], us)
    tally (MutationSurvived Nothing) (k, s, t, ss, ts, us) = (k, s + 1, t, ss, ts, us)
    tally (MutationSurvived (Just survivor)) (k, s, t, ss, ts, us) = (k, s + 1, t, ss ++ [survivor], ts, us)

    runOne defaultExe augDir sem record =
      bracket_ (waitQSem sem) (signalQSem sem) $ do
        let mid = augmentedMutationRecordId record
        hPutChunksLocaleWith (settingTerminalCapabilities settings) stderr (unlinesChunks (renderMutationProgressEvent (MutationProgressEvent record)))
        -- Only run suites that have at least one covering test for this mutation.
        let coveringBySuite =
              Map.filter (not . null) (augmentedMutationRecordCoveringTests record)
        if Map.null coveringBySuite
          then pure (MutationUncovered (UncoveredMutation record))
          else do
            -- Run one child per covering suite. The mutation is killed if any
            -- child exits non-zero; timed out (counted as killed) if any
            -- child exceeded its budget without any other child killing it
            -- first; otherwise survived.
            outcomes <- mapM (runOneSuite defaultExe augDir record mid) (Map.keys coveringBySuite)
            pure $ classifyOutcomes record outcomes

    classifyOutcomes record outcomes
      | any isKilled outcomes = MutationKilled
      | otherwise = case mTimedOut of
          Just (elapsedMicros, mLog) ->
            MutationTimedOut $
              fmap
                ( \relFile ->
                    TimedOutMutation
                      { timedOutMutationRecord = record,
                        timedOutMutationElapsedMicros = elapsedMicros,
                        timedOutMutationLogFile = relFile
                      }
                )
                mLog
          Nothing ->
            MutationSurvived $
              fmap
                ( \relFile ->
                    SurvivedMutation
                      { survivedMutationRecord = record,
                        survivedMutationLogFile = relFile
                      }
                )
                mSurvived
      where
        isKilled SuiteKilled = True
        isKilled _ = False
        mTimedOut = listToMaybe [(micros, mLog) | SuiteTimedOut micros mLog <- outcomes]
        mSurvived = listToMaybe [rf | SuiteSurvived (Just rf) <- outcomes]

    runOneSuite defaultExe augDir record mid suiteName = do
      let suiteExes = settingMutationSuiteExes settings
          exe = fromMaybe defaultExe (Map.lookup suiteName suiteExes)
          rtsArgs = case settingMutationChildMemLimit settings of
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
          logHandle <- openFile (fromAbsFile logPath) WriteMode
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
          outcomeRaw <- startProcessAndWait childProc micros
          endTime <- getCurrentTime
          let elapsedMicros = diffUTCTimeMicros endTime startTime
          hClose logHandle
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
      case settingMutationReportDir settings of
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
    -- pid and throws ECHILD.  Swallow that — the only reason we hold the
    -- bracket is to guarantee cleanup, which has already happened.
    startProcessAndWait childProc micros =
      bracket (startProcess childProc) cleanup $ \p -> do
        result <- race (threadDelay micros) (waitExitCode p)
        case result of
          Left () -> pure (Left ())
          Right ec -> pure (Right ec)
      where
        cleanup p =
          stopProcess p
            `Exception.catch` (\(_ :: IOException) -> pure ())

-- | Child process: run only the tests covering a single mutation and exit
-- with the appropriate exit code.
--
-- When @settingMutationSuiteName@ is set, only the covering tests for that
-- suite are run.  Otherwise the union of all suites' covering tests is used
-- (single-suite / backward-compatible behaviour).
runSingleMutationMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runSingleMutationMode settings _manifestDirs spec = do
  mid <- case settingMutationOne settings >>= parseMutationId of
    Nothing -> fail "runSingleMutationMode: no valid --mutation-one id"
    Just m -> pure m
  augDir <- resolveAugmentedManifestDir settings
  augmented <- readAugmentedManifestFile augDir
  specForest <- execTestDefM settings spec
  let coveringTestsMap =
        maybe
          Map.empty
          augmentedMutationRecordCoveringTests
          (lookupAugmentedMutationRecord mid augmented)
      coveringTests :: [TestId]
      coveringTests = case settingMutationSuiteName settings of
        Just suiteName ->
          fromMaybe [] (Map.lookup suiteName coveringTestsMap)
        Nothing ->
          -- single-suite / backward-compat: union of all suites
          concatMap snd (Map.toList coveringTestsMap)
      forest = case coveringTests of
        [] -> specForest
        ts -> filterTestForestByTrie (testIdTrieFromList ts) specForest
  setActiveMutation (Just mid)
  timedResult <- runSpecForestSynchronously (settings {settingThreads = Synchronous, settingFailFast = True}) forest
  setActiveMutation Nothing
  printOutputSpecForest settings timedResult
  if shouldExitFail settings (timedValue timedResult)
    then exitWith (ExitFailure 1)
    else exitSuccess

renderMutationRunReport :: MutationRunReport -> [[Chunk]]
renderMutationRunReport
  MutationRunReport
    { mutationRunReportKilled,
      mutationRunReportSurvived,
      mutationRunReportTimedOut,
      mutationRunReportUncovered,
      mutationRunReportSurvivors,
      mutationRunReportTimedOutMutations
    } =
    [ [chunk "Killed: ", fore green (chunk (T.pack (show mutationRunReportKilled)))],
      [chunk "  (of which timed out: ", fore yellow (chunk (T.pack (show mutationRunReportTimedOut))), chunk ")"],
      [chunk "Survived: ", fore red (chunk (T.pack (show mutationRunReportSurvived)))],
      [chunk "Uncovered: ", fore yellow (chunk (T.pack (show mutationRunReportUncovered)))]
    ]
      ++ ( if null mutationRunReportTimedOutMutations
             then []
             else
               [[], [chunk "Timed-out mutations:"]]
                 ++ concatMap renderTimedOut mutationRunReportTimedOutMutations
         )
      ++ ( if null mutationRunReportSurvivors
             then []
             else
               [[], [chunk "Surviving mutations:"]]
                 ++ concatMap renderSurvivor mutationRunReportSurvivors
         )
    where
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

renderMutationAddedEvent :: MutationAddedEvent -> [[Chunk]]
renderMutationAddedEvent MutationAddedEvent {mutationAddedRecord} =
  let MutationRecord
        { mutRecId = MutationId parts,
          mutRecOperator,
          mutRecOriginal,
          mutRecReplacement,
          mutRecSourceFile,
          mutRecSourceLines,
          mutRecMutatedLines,
          mutRecContextBefore,
          mutRecContextAfter,
          mutRecLine
        } = mutationAddedRecord
   in case parts of
        (modName : _op : lineStr : colStartStr : colEndStr : _) ->
          let filePath = case mutRecSourceFile of
                Just p -> fromRelFile p
                Nothing -> map (\c -> if c == '.' then '/' else c) modName ++ ".hs"
              variantSuffix = case parts of
                [_, _, _, _, _, _, altIdx] -> " #" ++ altIdx
                _ -> ""
              headerText = T.pack $ "added mutation " ++ T.unpack mutRecOperator ++ " at " ++ filePath ++ ":" ++ lineStr ++ ":" ++ colStartStr ++ "-" ++ colEndStr ++ variantSuffix
              headerLine = [chunk headerText]
           in case mutRecSourceLines of
                [] ->
                  [ headerLine,
                    [fore red (chunk ("    - " <> mutRecOriginal))],
                    [fore green (chunk ("    + " <> mutRecReplacement))]
                  ]
                _ ->
                  headerLine : renderUnifiedDiff (fromIntegral mutRecLine) mutRecContextBefore mutRecSourceLines mutRecMutatedLines mutRecContextAfter
        _ -> [[chunk (T.pack $ "added mutation " ++ intercalate "/" parts)]]

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
