{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Parent-side runner for the mutation phase.
--
-- Reads @manifest-augmented.json@ (produced by the coverage phase) and
-- spawns one mutation child per mutation per suite that covers it.  A
-- mutation is killed if any covering suite's child exits non-zero;
-- otherwise it is a survivor (or timed-out).
module Test.Syd.Mutation.Driver.Mutate
  ( runMutationMode,
    writeChildTask,
    scheduleGroups,
    UnknownCoveringSuite (..),
  )
where

import Control.Concurrent (newQSem, signalQSem, threadDelay, waitQSem)
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (bracket, bracket_)
import qualified Control.Exception as Exception
import qualified Data.ByteString as SB
import Data.Foldable (for_)
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Path
import Path.IO (copyFile, ensureDir, ignoringAbsence, withSystemTempDir)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), IOMode (..), hFlush, hSetBuffering, stderr, withFile)
import System.Process.Typed (proc, setStderr, setStdout, setWorkingDir, startProcess, stopProcess, useHandleOpen, waitExitCode)
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
    ControlFailedMutation (..),
    ControlTally (..),
    MutationGroupReport (..),
    MutationProgressEvent (..),
    MutationRunReport (..),
    MutationTally (..),
    SurvivedMutation (..),
    TimedOutMutation (..),
    UncoveredMutation (..),
    readAugmentedManifestFile,
    writeAugmentedManifestFile,
    writeMutationRunReport,
  )
import Test.Syd.Mutation.Driver.OptParse (SuiteConfig (..))
import Test.Syd.Mutation.Manifest (isControlOperator)
import Test.Syd.Mutation.Runtime (renderMutationId)
import Test.Syd.Mutation.TestBaselineMap
  ( TestBaselineMap (..),
    readTestBaselineMapDirIfExists,
    writeTestBaselineMapDir,
  )
import Test.Syd.MutationMode.Common
  ( MutationFailFast (..),
    MutationResult (..),
    OutcomeTally (..),
    SuiteOutcome (..),
    classifySyncExceptionAsKilled,
    diffMonotonicMicros,
    renderMutationProgressEvent,
    renderMutationRunReport,
    resultToOutcome,
    runOneGroup,
    tallyGroups,
  )
import Text.Colour (Chunk, TerminalCapabilities (..), hPutChunksUtf8With, putChunksUtf8With, renderChunksText, unlinesChunks)

-- | Thrown when the augmented manifest references a covering suite that is
-- not present in the driver's suite-exe map.  Validated up-front in
-- 'runMutationMode' so the failure surfaces before any mutation child is
-- spawned, instead of as an 'ErrorCall' from inside a 'mapConcurrently'
-- worker.
data UnknownCoveringSuite = UnknownCoveringSuite
  { unknownCoveringSuiteName :: !Text,
    unknownCoveringSuiteDeclared :: ![Text]
  }
  deriving (Show)

instance Exception.Exception UnknownCoveringSuite

-- | Render @report.txt@ to @<outDir>/report.txt@.  Writes bytes
-- directly so we don't depend on the locale's encoding being UTF-8
-- (the report contains box-drawing characters and can embed non-ASCII
-- source lines from the diffs it renders).
writeReportTxt :: [[Chunk]] -> Path Abs Dir -> IO ()
writeReportTxt renderedChunks outDir =
  let renderedText = renderChunksText With8BitColours (unlinesChunks renderedChunks)
      reportFile = outDir </> [relfile|report.txt|]
   in SB.writeFile (fromAbsFile reportFile) (TE.encodeUtf8 renderedText)

-- | The order to start the mutation groups of a run in, most expensive first.
--
-- Every group waits for a slot, so the order they are handed to the runner in
-- is the order they get one.  A run's groups differ by orders of magnitude:
-- most are a second, a few are minutes, because a group costs what the covering
-- tests it runs cost.  Started last, one of those few holds a slot while the
-- rest of them stand idle, so a run ends with one child and a quiet machine.
--
-- The cost here is what the covering tests cost on their own, which is what
-- the per-mutation budgets are derived from as well.  It is an estimate: a
-- mutation stops at the test that kills it, and which test that is cannot be
-- known before running it.  Scheduling is all it decides, so an estimate is
-- enough, and it leaves every verdict and the reported order alone.
scheduleGroups :: Maybe TestBaselineMap -> [(Int, AugmentedMutationGroup)] -> [(Int, AugmentedMutationGroup)]
scheduleGroups mBaselines = sortOn (Down . groupCost)
  where
    costs = maybe Map.empty (\(TestBaselineMap m) -> m) mBaselines
    groupCost :: (Int, AugmentedMutationGroup) -> Word
    groupCost (_, AugmentedMutationGroup recs) =
      sum
        [ Map.findWithDefault 0 t costs
        | r <- recs,
          ts <- Map.elems (augmentedMutationRecordCoveringTests r),
          t <- ts
        ]

-- | Write the directory one mutation child is pointed at: the mutation it is
-- being asked about, and the baselines of the tests it will consider.
--
-- A child needs only its own record, and parsing the whole manifest costs time
-- and memory linear in its size, once per mutation per covering suite.
--
-- A run with no baselines gets no file at all: an empty map still reorders.
writeChildTask :: Maybe TestBaselineMap -> AugmentedMutationRecord -> Text -> Path Abs Dir -> IO ()
writeChildTask mBaselines record suiteName childAugDir = do
  writeAugmentedManifestFile
    childAugDir
    (AugmentedManifest [AugmentedMutationGroup [record]])
  let coveringHere =
        Map.findWithDefault
          []
          suiteName
          (augmentedMutationRecordCoveringTests record)
  for_ mBaselines $ \(TestBaselineMap allBaselines) ->
    writeTestBaselineMapDir
      childAugDir
      (TestBaselineMap (Map.restrictKeys allBaselines (Set.fromList coveringHere)))

-- | Count a mutation that is about to be tested, and write its progress line.
--
-- The number a mutation is shown under is how many have started, itself
-- included, so the counter climbs by one per line and a reader can see how far
-- along a run is.  It is not an identifier: which mutation gets which number
-- depends on the order the concurrent workers happen to reach here in, and
-- every mutation the report has anything to say about is named there by id.
--
-- Taking the number and writing the line happen under the same lock, so the
-- numbers reach the log in the order they were handed out, and a debug run's
-- multi-line diffs arrive whole instead of interleaved.
emitMutationProgress ::
  -- | How many mutations have started.
  MVar Int ->
  -- | Debug mode: follow the line with the mutation's source diff.
  Bool ->
  -- | How many mutations the run tests in total.
  Int ->
  AugmentedMutationRecord ->
  IO ()
emitMutationProgress startedVar debug total record =
  modifyMVar_ startedVar $ \started -> do
    let index = started + 1
    hPutChunksUtf8With
      With8BitColours
      stderr
      (unlinesChunks (renderMutationProgressEvent debug index total (MutationProgressEvent record)))
    pure index

-- | Parent process: read @manifest-augmented.json@ and spawn one child
-- subprocess per mutation per suite that covers it.
--
-- Each child receives @--mutation-one <id> --mutation-suite-name <suite>
-- --mutation-augmented-manifest-dir <dir>@ and exits 0 (survived) or
-- non-zero (killed).  The directory is one written for that child alone
-- ('writeChildTask'), not the run's own.
--
-- Suites with no covering tests for a given mutation are skipped — running
-- a suite with an empty filter would cause sydtest to run all tests.
--
-- Also writes @report.json@ to the configured report directory.
runMutationMode ::
  -- | Whether to abort on the first surviving or uncovered mutation.
  Bool ->
  -- | Debug mode.  A concise one-line progress message is printed for
  -- every mutation regardless, so a long run shows steady activity; in
  -- debug mode each of those lines is followed by the mutation's full
  -- source diff.  Off by default because that per-mutation diff — for
  -- the killed mutations that are the overwhelming majority of a healthy
  -- run — floods the build log and buries the survivors; the final
  -- report still lists every survivor in full.
  Bool ->
  -- | Augmented-manifest directory.
  Path Abs Dir ->
  -- | Output directory: report.json, report.txt, and per-suite *.log
  -- files are written here.
  Path Abs Dir ->
  -- | Optional RTS heap cap to apply to each mutation child.
  Maybe String ->
  -- | Maximum number of mutation children to run concurrently.  'Nothing'
  -- uses 'getNumCapabilities'.  Capping this matters for suites that spin up
  -- a resource per test (e.g. a tmp-postgres): at full core-count
  -- concurrency that resource flakes under contention, which surfaces as a
  -- spuriously-failing test and therefore a /false kill/ — non-reproducible
  -- and inconsistent with a lower-volume diff-scoped run.  Mirrors the
  -- coverage phase's @--coverage-jobs@.
  Maybe Word ->
  -- | Map of suite name to its config (exe + resource dir).  The exe is
  -- spawned for each covering suite; the resource dir becomes the child's
  -- working directory, so a mutation child enumerates the same spec forest
  -- and resolves the same relative resource paths that the coverage phase
  -- did — without which the recorded covering-test ids may not match the
  -- forest the child sees, producing false survivors.
  Map.Map Text SuiteConfig ->
  IO MutationRunReport
runMutationMode failFast debug augDir outDir childMemLimit mutationJobs suiteConfigs = do
  hSetBuffering stderr (BlockBuffering Nothing)
  ensureDir outDir
  AugmentedManifest groups <- readAugmentedManifestFile augDir
  -- Validate that every covering-suite name in the manifest is in
  -- 'suiteConfigs' before any worker spawns.  An unknown name would otherwise
  -- surface as an 'ErrorCall' from inside a 'mapConcurrently' worker, which
  -- is harder to attribute.
  let referenced =
        Map.unions
          [ augmentedMutationRecordCoveringTests r
          | AugmentedMutationGroup rs <- groups,
            r <- rs
          ]
      missing = Map.difference referenced suiteConfigs
  case Map.keys missing of
    [] -> pure ()
    (name : _) ->
      Exception.throwIO
        UnknownCoveringSuite
          { unknownCoveringSuiteName = name,
            unknownCoveringSuiteDeclared = Map.keys suiteConfigs
          }
  -- Read once here rather than once per child; see 'writeChildTask'.
  baselines <- readTestBaselineMapDirIfExists augDir
  n <- case mutationJobs of
    Just j | j > 0 -> pure (fromIntegral j)
    _ -> getNumCapabilities
  sem <- newQSem n
  let totalMutations = sum [length recs | AugmentedMutationGroup recs <- groups]
  startedVar <- newMVar (0 :: Int)
  -- Each group's per-mutation results, accumulated in source order so a
  -- partial report (after a global fail-fast abort) reflects the work
  -- done.
  groupResultsVar <- newTVarIO (Map.empty :: Map.Map Int [MutationResult])
  let runGroup' (gix, AugmentedMutationGroup recs) =
        runOneGroup
          failFast
          (runOne baselines startedVar totalMutations sem)
          ( \r ->
              atomically $
                modifyTVar' groupResultsVar (Map.insertWith (++) gix [r])
          )
          recs
  -- Catch every exception so the partial report (whatever results have
  -- already landed in 'groupResultsVar') is still written out.  Re-throw
  -- after the report write unless the exception is the expected
  -- 'MutationFailFast' fast-path.
  mWorkerException <-
    Exception.try @Exception.SomeException $ do
      _ <- mapConcurrently runGroup' (scheduleGroups baselines (zip [0 :: Int ..] groups))
      pure ()
  finalGroupResults <- readTVarIO groupResultsVar
  -- Preserve original group order; reverse each group's results because
  -- they were accumulated cons-style.
  let groupReports =
        [ MutationGroupReport (map resultToOutcome (reverse (Map.findWithDefault [] gix finalGroupResults)))
        | gix <- [0 .. length groups - 1]
        ]
      OutcomeTally
        { tallyKilled = killed,
          tallySurvived = survived,
          tallyTimedOut = timedOut,
          tallyUncovered = uncovered,
          tallySkipped = skipped,
          tallyControlPassed = controlPassed,
          tallyControlFailed = controlFailed
        } = tallyGroups groupReports
      jsonReport =
        MutationRunReport
          { mutationRunReportMutations =
              MutationTally
                { mutationTallyKilled = killed,
                  mutationTallySurvived = survived,
                  mutationTallyTimedOut = timedOut,
                  mutationTallyUncovered = uncovered,
                  mutationTallySkipped = skipped
                },
            mutationRunReportControls =
              ControlTally
                { controlTallyPassed = controlPassed,
                  controlTallyFailed = controlFailed
                },
            mutationRunReportGroups = groupReports
          }
  writeMutationRunReport outDir jsonReport
  -- Render the report once and write report.txt alongside report.json
  -- in the out dir.  Done here (rather than in 'runDriver') so that a
  -- --fail-fast exitWith below still produces report.txt — without
  -- this, fail-fast would skip the report.txt write because control
  -- never returns to 'runDriver'.
  let renderedChunks = renderMutationRunReport jsonReport
  writeReportTxt renderedChunks outDir
  putChunksUtf8With With8BitColours (unlinesChunks renderedChunks)
  -- Force out any block-buffered progress events before we return.  This
  -- matters even though we don.t .exitWith. here ourselves: callers (e.g.
  -- .runDriver. under --fail-fast) may, and a buffered stderr line that
  -- lands after the caller.s stdout summary block would look like trailing
  -- garbage in the build log.
  hFlush stderr
  case mWorkerException of
    Left e -> case Exception.fromException e of
      Just MutationFailFast -> pure ()
      Nothing -> Exception.throwIO e
    Right () -> pure ()
  -- Note: 'runMutationMode' itself does NOT 'exitWith' here, even under
  -- --fail-fast.  Returning the report lets callers print their own final
  -- summary line and decide the exit code.  Callers that want the
  -- historical fail-fast exit (e.g. the full-report 'runDriver') call
  -- 'exitWith (ExitFailure 1)' themselves on a non-empty survived/uncovered
  -- count.  This is what lets 'runDiff' still print its PASS/FAIL block
  -- when fail-fast trips.
  pure jsonReport
  where
    runOne baselines startedVar totalMutations sem record =
      bracket_ (waitQSem sem) (signalQSem sem) $ do
        let mid = augmentedMutationRecordId record
        emitMutationProgress startedVar debug totalMutations record
        -- Only run suites that have at least one covering test for this
        -- mutation.
        let coveringBySuite =
              Map.filter (not . null) (augmentedMutationRecordCoveringTests record)
        case NE.nonEmpty (Map.keys coveringBySuite) of
          Nothing -> pure (MutationUncovered (UncoveredMutation record))
          Just suiteNames -> do
            -- Run one child per covering suite.  The mutation is killed
            -- if any child exits non-zero; timed out if any child exceeded
            -- its budget without any other child killing it first;
            -- otherwise survived.
            outcomes <- runSuitesRetryingOverruns baselines record mid suiteNames attemptsOnOverrun
            -- A control (no-op) mutation is expected to survive.  Reinterpret
            -- its raw outcome: survival is the control passing, a kill or
            -- timeout is the control failing (the suite is unsound).
            pure $
              if isControlOperator (augmentedMutationRecordOperator record)
                then asControlResult outcomes (classifyOutcomes record outcomes)
                else classifyOutcomes record outcomes

    -- How many times a mutation may overrun its budget before the overrun is
    -- taken at face value.
    attemptsOnOverrun :: Word
    attemptsOnOverrun = 3

    -- Run every covering suite, repeating only while the run tells us
    -- nothing.
    --
    -- A kill and a survival are both observations and stand on the first run.
    -- Repeating either would bias the score towards whichever outcome we
    -- repeated into, which is the trap sydtest's own 'flaky' warns about from
    -- the other side: retrying a survivor until something fails scores the
    -- mutation as caught on the strength of an accidental failure.
    --
    -- An overrun is not an observation. The budget is ten times a baseline
    -- measured on an idle machine, so a loaded one exceeds it with nothing
    -- wrong, and re-running replaces a non-observation with a real one
    -- without favouring either outcome. That is worth doing here rather than
    -- leaving to whoever re-runs the check, because the run this belongs to
    -- costs the better part of an hour.
    --
    -- [tag:TimeoutIsAKill] An overrun that survives these attempts is scored
    -- as a kill, in 'tallyGroups', and left alone by 'isMutationFailure'.
    -- Both of those are sound only because of the retry here: one overrun
    -- says nothing, since a busy machine exceeds a budget derived from an
    -- idle one, and crediting that would let load raise the score. Weakening
    -- or removing these attempts means revisiting how an overrun is scored.
    runSuitesRetryingOverruns baselines record mid suiteNames attemptsLeft = do
      outcomes <- mapM (runOneSuite baselines record mid) suiteNames
      case classifyOutcomes record outcomes of
        MutationTimedOut _
          | attemptsLeft > 1 ->
              runSuitesRetryingOverruns baselines record mid suiteNames (attemptsLeft - 1)
        _ -> pure outcomes

    -- Map a control mutation's raw classification onto the control-specific
    -- results.  An uncovered control never reaches here (it short-circuits
    -- above), and 'classifyOutcomes' only ever yields killed/timed-out/survived.
    asControlResult outcomes = \case
      MutationSurvived sm -> MutationControlPassed (survivedMutationRecord sm)
      MutationKilled record ->
        MutationControlFailed (ControlFailedMutation record (killingSuiteLog outcomes))
      MutationTimedOut tm ->
        MutationControlFailed
          (ControlFailedMutation (timedOutMutationRecord tm) (timedOutMutationLogFile tm))
      other -> other

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
                  -- Prefer a survivor suite that produced a log file;
                  -- fall back to no log otherwise.  By non-emptiness of
                  -- @outcomes@ and the branches above, at least one
                  -- element is 'SuiteSurvived'.
                  survivedMutationLogFile =
                    listToMaybe [rf | SuiteSurvived (Just rf) <- NE.toList outcomes]
                }
      where
        isKilled (SuiteKilled _) = True
        isKilled _ = False
        mTimedOut = listToMaybe [(micros, mLog) | SuiteTimedOut micros mLog <- NE.toList outcomes]

    -- \| The log of the first suite that killed the mutation, when one was
    -- kept.  Only a control's kill keeps one (see 'runOneSuite'), which is
    -- also the only caller.
    killingSuiteLog outcomes =
      listToMaybe [rf | SuiteKilled (Just rf) <- NE.toList outcomes]

    runOneSuite baselines record mid suiteName = do
      (exe, mResourceDir) <- case Map.lookup suiteName suiteConfigs of
        Just SuiteConfig {suiteConfigExe, suiteConfigResourceDir} ->
          pure (fromAbsFile suiteConfigExe, suiteConfigResourceDir)
        Nothing ->
          -- Should be unreachable: 'runMutationMode' validates up-front
          -- that every covering-suite name in the manifest is in
          -- 'suiteConfigs'.  If this fires, the manifest is being mutated
          -- between that check and this lookup, or the validation has a
          -- bug — either way, throw an attributable exception rather
          -- than 'error'.
          Exception.throwIO
            UnknownCoveringSuite
              { unknownCoveringSuiteName = suiteName,
                unknownCoveringSuiteDeclared = Map.keys suiteConfigs
              }
      let rtsArgs = case childMemLimit of
            Nothing -> []
            Just limit -> ["+RTS", "-M" ++ limit, "-RTS"]
          suiteNameStr = T.unpack suiteName
      -- Only the run of the child is classified: an exception from preparing
      -- its directory says nothing about whether a test noticed the mutation.
      withSystemTempDir "mutation-child" $ \tmpDir -> do
        let logPath = tmpDir </> [relfile|child.log|]
        let childAugDir = tmpDir </> [reldir|augmented|]
        writeChildTask baselines record suiteName childAugDir
        let args =
              [ "--mutation-one",
                renderMutationId mid,
                "--mutation-augmented-manifest-dir",
                fromAbsDir childAugDir,
                "--mutation-suite-name",
                suiteNameStr
              ]
                ++ rtsArgs
        classifySyncExceptionAsKilled $ do
          (outcomeRaw, elapsedMicros) <-
            withFile (fromAbsFile logPath) WriteMode $ \logHandle -> do
              let childProc =
                    -- Run the child in the suite's resource directory, so it
                    -- enumerates the same spec forest (and resolves the same
                    -- relative resource paths) the coverage phase recorded
                    -- covering-test ids against.
                    maybe id (setWorkingDir . fromAbsDir) mResourceDir $
                      setStdout (useHandleOpen logHandle) $
                        setStderr (useHandleOpen logHandle) $
                          proc exe args
                  -- Per-mutation monotonic-clock budget computed by the
                  -- coverage phase.
                  timeoutMicros = augmentedMutationRecordTimeoutMicros record
                  -- Cap the threadDelay argument at maxBound Int so very
                  -- large budgets don't overflow when converted to the
                  -- Int that threadDelay expects.
                  micros =
                    if timeoutMicros >= fromIntegral (maxBound :: Int)
                      then maxBound :: Int
                      else fromIntegral timeoutMicros
              startTime <- getMonotonicTimeNSec
              raw <- startProcessAndWait childProc micros
              endTime <- getMonotonicTimeNSec
              pure (raw, diffMonotonicMicros endTime startTime)
          case outcomeRaw of
            Left () -> do
              -- Timed out: parent killed the child.  Preserve whatever
              -- the child managed to write so the report retains useful
              -- context.
              mRelFile <- copyChildLog "timeout-" mid suiteName logPath
              pure (SuiteTimedOut elapsedMicros mRelFile)
            Right ec -> case ec of
              ExitFailure _ -> do
                -- A kill is the expected outcome and its output says nothing,
                -- except for a control: a no-op cannot legitimately be killed,
                -- so the child's output is the only place that names the test
                -- which killed it, and without it the flaky test behind a
                -- control failure cannot be found.
                mRelFile <-
                  if isControlOperator (augmentedMutationRecordOperator record)
                    then copyChildLog "control-failure-" mid suiteName logPath
                    else pure Nothing
                pure (SuiteKilled mRelFile)
              ExitSuccess -> do
                mRelFile <- copyChildLog "survivor-" mid suiteName logPath
                pure (SuiteSurvived mRelFile)

    copyChildLog prefix mid suiteName logPath = do
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
          copyFile logPath (outDir </> relFile)
          pure (Just relFile)

    -- Race the child against a delay; on timeout, stop the process
    -- (SIGTERM via System.Process.Typed's stopProcess; SIGKILL follows
    -- after the library's grace period) and report a Left (timeout)
    -- outcome.
    --
    -- bracket guarantees the child is reaped on both the timeout-wins
    -- branch and on async exceptions propagating into this thread.  When
    -- the inner branch wins, the child has already been reaped by
    -- 'waitExitCode'; the cleanup's 'stopProcess' then calls
    -- 'waitForProcess' on an already-reaped pid and throws ECHILD.
    -- 'ignoringAbsence' silences exactly that not-found case and rethrows
    -- anything else.
    startProcessAndWait childProc micros =
      bracket (startProcess childProc) (ignoringAbsence . stopProcess) $ \p -> do
        result <- race (threadDelay micros) (waitExitCode p)
        case result of
          Left () -> pure (Left ())
          Right ec -> pure (Right ec)
