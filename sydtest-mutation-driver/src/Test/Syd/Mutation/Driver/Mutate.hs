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
    UnknownCoveringSuite (..),
  )
where

import Control.Concurrent (newQSem, signalQSem, threadDelay, waitQSem)
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (bracket, bracket_)
import qualified Control.Exception as Exception
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Path
import Path.IO (copyFile, ignoringAbsence, withSystemTempDir)
import System.Exit (ExitCode (..), exitWith)
import System.IO (BufferMode (..), IOMode (..), hFlush, hSetBuffering, stderr, withFile)
import System.Process.Typed (proc, setStderr, setStdout, startProcess, stopProcess, useHandleOpen, waitExitCode)
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
    MutationGroupReport (..),
    MutationProgressEvent (..),
    MutationRunReport (..),
    SurvivedMutation (..),
    TimedOutMutation (..),
    UncoveredMutation (..),
    readAugmentedManifestFile,
    writeMutationRunReport,
  )
import Test.Syd.Mutation.Runtime (renderMutationId)
import Test.Syd.MutationMode.Common
  ( MutationFailFast (..),
    MutationResult (..),
    OutcomeTally (..),
    SuiteOutcome (..),
    classifySyncExceptionAsKilled,
    diffMonotonicMicros,
    renderMutationProgressEvent,
    renderMutationRunReport,
    resolveAugmentedManifestDir,
    resultToOutcome,
    runOneGroup,
    tallyGroups,
  )
import Text.Colour (Chunk, TerminalCapabilities (..), hPutChunksLocaleWith, putChunksLocaleWith, renderChunksText, unlinesChunks)

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
-- (the report contains em-dashes and box-drawing characters).
writeReportTxt :: [[Chunk]] -> Path Abs Dir -> IO ()
writeReportTxt renderedChunks outDir =
  let renderedText = renderChunksText With8BitColours (unlinesChunks renderedChunks)
      reportFile = outDir </> [relfile|report.txt|]
   in BS.writeFile (fromAbsFile reportFile) (TE.encodeUtf8 renderedText)

-- | Parent process: read @manifest-augmented.json@ and spawn one child
-- subprocess per mutation per suite that covers it.
--
-- Each child receives @--mutation-one <id> --mutation-suite-name <suite>
-- --mutation-augmented-manifest-dir <dir>@ and exits 0 (survived) or
-- non-zero (killed).
--
-- Suites with no covering tests for a given mutation are skipped — running
-- a suite with an empty filter would cause sydtest to run all tests.
--
-- Also writes @report.json@ to the configured report directory.
runMutationMode ::
  -- | Whether to abort on the first surviving or uncovered mutation.
  Bool ->
  -- | Optional augmented-manifest directory.  'Nothing' means CWD.
  Maybe (Path Abs Dir) ->
  -- | Optional report directory.  'Nothing' skips writing report files.
  Maybe (Path Abs Dir) ->
  -- | Optional RTS heap cap to apply to each mutation child.
  Maybe String ->
  -- | Map of suite name to exe path; used to find each covering suite's
  -- exe when spawning a mutation child.
  Map.Map Text (Path Abs File) ->
  IO MutationRunReport
runMutationMode failFast augmentedManifestDirM reportDirM childMemLimit suiteExes = do
  hSetBuffering stderr (BlockBuffering Nothing)
  augDir <- resolveAugmentedManifestDir augmentedManifestDirM
  AugmentedManifest groups <- readAugmentedManifestFile augDir
  -- Validate that every covering-suite name in the manifest is in
  -- 'suiteExes' before any worker spawns.  An unknown name would otherwise
  -- surface as an 'ErrorCall' from inside a 'mapConcurrently' worker, which
  -- is harder to attribute.
  let referenced =
        Map.unions
          [ augmentedMutationRecordCoveringTests r
          | AugmentedMutationGroup rs <- groups,
            r <- rs
          ]
      missing = Map.difference referenced suiteExes
  case Map.keys missing of
    [] -> pure ()
    (name : _) ->
      Exception.throwIO
        UnknownCoveringSuite
          { unknownCoveringSuiteName = name,
            unknownCoveringSuiteDeclared = Map.keys suiteExes
          }
  n <- getNumCapabilities
  sem <- newQSem n
  -- Each group's per-mutation results, accumulated in source order so a
  -- partial report (after a global fail-fast abort) reflects the work
  -- done.
  groupResultsVar <- newTVarIO (Map.empty :: Map.Map Int [MutationResult])
  let runGroup' (gix, AugmentedMutationGroup recs) =
        runOneGroup
          failFast
          (runOne augDir sem)
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
      _ <- mapConcurrently runGroup' (zip [0 :: Int ..] groups)
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
  mapM_ (`writeMutationRunReport` jsonReport) reportDirM
  -- Render the report once and write report.txt alongside report.json
  -- in the out dir.  Done here (rather than in 'runDriver') so that a
  -- --fail-fast exitWith below still produces report.txt — without
  -- this, fail-fast would skip the report.txt write because control
  -- never returns to 'runDriver'.
  let renderedChunks = renderMutationRunReport jsonReport
  mapM_ (writeReportTxt renderedChunks) reportDirM
  putChunksLocaleWith With8BitColours (unlinesChunks renderedChunks)
  -- Force out any block-buffered progress events before we either return
  -- normally or exit non-zero.  Without this, the last few lines of
  -- progress can be dropped when 'exitWith' tears down the runtime.
  hFlush stderr
  case mWorkerException of
    Left e -> case Exception.fromException e of
      Just MutationFailFast -> pure ()
      Nothing -> Exception.throwIO e
    Right () -> pure ()
  when (failFast && (survived > 0 || uncovered > 0)) $ exitWith (ExitFailure 1)
  pure jsonReport
  where
    runOne augDir sem record =
      bracket_ (waitQSem sem) (signalQSem sem) $ do
        let mid = augmentedMutationRecordId record
        hPutChunksLocaleWith With8BitColours stderr (unlinesChunks (renderMutationProgressEvent (MutationProgressEvent record)))
        -- Only run suites that have at least one covering test for this
        -- mutation.
        let coveringBySuite =
              Map.filter (not . null) (augmentedMutationRecordCoveringTests record)
        case NE.nonEmpty (Map.keys coveringBySuite) of
          Nothing -> pure (MutationUncovered (UncoveredMutation record))
          Just suiteNames -> do
            -- Run one child per covering suite.  The mutation is killed
            -- if any child exits non-zero; timed out (counted as killed)
            -- if any child exceeded its budget without any other child
            -- killing it first; otherwise survived.
            outcomes <- mapM (runOneSuite augDir record mid) suiteNames
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
                  -- Prefer a survivor suite that produced a log file;
                  -- fall back to no log otherwise.  By non-emptiness of
                  -- @outcomes@ and the branches above, at least one
                  -- element is 'SuiteSurvived'.
                  survivedMutationLogFile =
                    listToMaybe [rf | SuiteSurvived (Just rf) <- NE.toList outcomes]
                }
      where
        isKilled SuiteKilled = True
        isKilled _ = False
        mTimedOut = listToMaybe [(micros, mLog) | SuiteTimedOut micros mLog <- NE.toList outcomes]

    runOneSuite augDir record mid suiteName = do
      exe <- case Map.lookup suiteName suiteExes of
        Just e -> pure (fromAbsFile e)
        Nothing ->
          -- Should be unreachable: 'runMutationMode' validates up-front
          -- that every covering-suite name in the manifest is in
          -- 'suiteExes'.  If this fires, the manifest is being mutated
          -- between that check and this lookup, or the validation has a
          -- bug — either way, throw an attributable exception rather
          -- than 'error'.
          Exception.throwIO
            UnknownCoveringSuite
              { unknownCoveringSuiteName = suiteName,
                unknownCoveringSuiteDeclared = Map.keys suiteExes
              }
      let rtsArgs = case childMemLimit of
            Nothing -> []
            Just limit -> ["+RTS", "-M" ++ limit, "-RTS"]
          suiteNameStr = T.unpack suiteName
          args =
            [ "--mutation-one",
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
                  -- Per-mutation wall-clock budget computed by the
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
              ExitFailure _ -> pure SuiteKilled
              ExitSuccess -> do
                mRelFile <- copyChildLog "survivor-" mid suiteName logPath
                pure (SuiteSurvived mRelFile)

    copyChildLog prefix mid suiteName logPath =
      case reportDirM of
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
