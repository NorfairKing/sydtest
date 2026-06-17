{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Utilities and types shared by the in-tree mutation child entry points
-- ('Test.Syd.MutationMode.Single', 'Test.Syd.MutationMode.SingleCoverage')
-- and the out-of-tree mutation driver (@sydtest-mutation-driver@).
--
-- Everything in this module is parent-vs-child neutral: it lives in
-- @sydtest@ because both the child entry points and the driver need it.
module Test.Syd.MutationMode.Common
  ( -- * Mutation outcomes
    MutationResult (..),
    SuiteOutcome (..),
    OutcomeTally (..),
    emptyOutcomeTally,
    tallyGroups,
    classifySyncExceptionAsKilled,
    retryingIO,
    isMutationFailure,
    mutationResultId,
    resultToOutcome,
    runOneGroup,

    -- * Exceptions
    MutationFailFast (..),
    CoverageFailFast (..),

    -- * Reporting
    renderMutationRunReport,
    renderMutationProgressEvent,
    renderUnifiedDiff,
    formatMutationLog,

    -- * Coverage progress events
    CoverageProgressEvent (..),
    CoverageProgressTestEvent (..),
    CoverageProgressPhase (..),
    CoverageProgressSkipReason (..),
    renderCoverageProgressEvent,
    emitCoverageEvent,

    -- * Timing utilities
    diffMonotonicMicros,
  )
where

import Control.Exception (Exception)
import qualified Control.Exception as Exception
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Word (Word64)
import Path
import System.IO (stderr)
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedMutationRecord (..),
    MutationGroupReport (..),
    MutationOutcome (..),
    MutationProgressEvent (..),
    MutationRunReport (..),
    SkippedMutation (..),
    SurvivedMutation (..),
    TimedOutMutation (..),
    UncoveredMutation (..),
    mutationGroupReportOutcomes,
  )
import Test.Syd.Mutation.Manifest.Render (renderUnifiedDiff)
import Test.Syd.Mutation.Runtime (MutationId (..), renderMutationId)
import Test.Syd.Mutation.TestId (TestId, renderTestId)
import Test.Syd.OptParse (Settings, settingTerminalCapabilities)
import Text.Colour (Chunk, chunk, cyan, fore, green, hPutChunksLocaleWith, red, unlinesChunks, yellow)

-- | Difference of two 'getMonotonicTimeNSec' readings expressed in
-- microseconds.  The monotonic clock is not affected by NTP slew or
-- step, so timing comparisons here are robust against system-clock
-- changes that 'getCurrentTime' would have observed.
diffMonotonicMicros :: Word64 -> Word64 -> Word
diffMonotonicMicros end start =
  -- 'getMonotonicTimeNSec' is monotonically non-decreasing, so @end >=
  -- start@ holds whenever they were measured in this order.  Guard
  -- with a defensive max anyway, in case a future change captures the
  -- two times across an unexpected boundary.
  fromIntegral ((max end start - start) `div` 1000)

data MutationResult
  = MutationUncovered UncoveredMutation
  | MutationKilled AugmentedMutationRecord
  | -- | At least one suite's child exceeded its monotonic-clock timeout. The
    -- mutation is counted as killed in the overall score but also recorded
    -- separately in the report for visibility.
    MutationTimedOut TimedOutMutation
  | MutationSurvived SurvivedMutation
  | -- | The mutation was not tested because an earlier mutation in the same
    -- group already failed (survived or was uncovered).  Within-group
    -- fail-fast records every remaining alternative as 'MutationSkipped'
    -- without spawning a child.
    MutationSkipped SkippedMutation
  deriving (Eq, Show)

-- | Per-suite outcome of running a single mutation child.
data SuiteOutcome
  = -- | Child exited non-zero — mutation killed by this suite.
    SuiteKilled
  | -- | Child exited zero — mutation survived in this suite. The optional
    -- log path points at the captured stdout/stderr (when a report dir is
    -- configured).
    SuiteSurvived (Maybe (Path Rel File))
  | -- | Child exceeded its monotonic-clock timeout and was terminated by the
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

tallyGroups :: [MutationGroupReport] -> OutcomeTally
tallyGroups = foldr (\(MutationGroupReport os) acc -> foldr step acc os) emptyOutcomeTally
  where
    step = \case
      OutcomeKilled _ -> \t -> t {tallyKilled = tallyKilled t + 1}
      OutcomeTimedOut _ -> \t -> t {tallyKilled = tallyKilled t + 1, tallyTimedOut = tallyTimedOut t + 1}
      OutcomeSurvived _ -> \t -> t {tallySurvived = tallySurvived t + 1}
      OutcomeUncovered _ -> \t -> t {tallyUncovered = tallyUncovered t + 1}
      OutcomeSkipped _ -> \t -> t {tallySkipped = tallySkipped t + 1}

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
      let failFastNow = globalFailFast && isMutationFailure r
      if failFastNow
        then do
          -- Record every remaining record in this group as skipped so the
          -- partial report (assembled by the caller after catching
          -- 'MutationFailFast') reflects the entire group, not just the
          -- records processed before the abort.
          loop (mutationResultId r) rest
          Exception.throwIO MutationFailFast
        else loop (mutationResultId r) rest

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
          [chunk "       disabled-mutations: [<Operator>]"],
          [chunk "  3. Suppress an equivalent mutant: if the operator swapped the arguments of a"],
          [chunk "     symmetric function (SwitchFunctionArguments on e.g. max or set union), no test"],
          [chunk "     can kill it.  List the function under the operator's skip-calls-to config key:"],
          [chunk "       operators: {SwitchFunctionArguments: {skip-calls-to: [<function>]}}"]
        ]
      remediationUncoveredBody =
        [ [chunk "  1. Cover it: add a test that exercises the mutation site so the coverage phase records a covering test."],
          [chunk "  2. Disable it: same annotations and config keys as for survivors above."]
        ]

-- | Render the per-mutation progress line emitted as each mutation is
-- tested.  The first argument is whether to be verbose: in concise mode
-- (the default) this is the single locating line @Testing mutation
-- \<operator\> at \<file\>:\<line\>:\<cols\>@, so a long run still shows
-- steady progress without flooding the log; in verbose\/debug mode the
-- full source diff of the mutation follows — the same block the report
-- prints for a survivor.
renderMutationProgressEvent :: Bool -> MutationProgressEvent -> [[Chunk]]
renderMutationProgressEvent verbose (MutationProgressEvent rec) =
  let logLines = formatMutationLog (augmentedMutationRecordId rec) rec
      withPrefix = case logLines of
        [] -> [[chunk "Testing mutation"]]
        (firstLine : rest) -> (chunk "Testing mutation " : firstLine) : rest
   in if verbose then withPrefix else take 1 withPrefix

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

-- 'renderUnifiedDiff' moved to 'Test.Syd.Mutation.Manifest.Render' so it
-- is shared with the plugin's @.txt@ manifest writer.
