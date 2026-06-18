{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.Mutation.AugmentedManifest
  ( AugmentedMutationRecord (..),
    AugmentedMutationGroup (..),
    AugmentedManifest (..),
    mergeAugmentedManifests,
    readAndUnionCoverageDirs,
    filterAugmentedManifestByIds,
    writeAugmentedManifestFile,
    readAugmentedManifestFile,
    readAugmentedManifestFileIfExists,
    lookupAugmentedMutationRecord,
    fromMutationRecord,
    defaultTimeoutMicros,
    SurvivedMutation (..),
    TimedOutMutation (..),
    UncoveredMutation (..),
    SkippedMutation (..),
    ControlFailedMutation (..),
    MutationOutcome (..),
    MutationGroupReport (..),
    MutationTally (..),
    ControlTally (..),
    MutationRunReport (..),
    writeMutationRunReport,
    readMutationRunReport,
    MutationRunReportDecodeException (..),
    MutationProgressEvent (..),
  )
where

import Autodocodec
import Control.Exception (Exception, throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.GenValidity.Map ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.Generics (Generic)
import Path
import Path.IO (ensureDir, forgivingAbsence)
import Test.Syd.Mutation.Manifest (MutationRecord (..), relFileCodec)
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId)

-- | A mutation record augmented with coverage data.
-- Unlike 'MutationRecord', covering tests are always present (never Nothing).
data AugmentedMutationRecord = AugmentedMutationRecord
  { augmentedMutationRecordId :: MutationId,
    augmentedMutationRecordOperator :: Text,
    augmentedMutationRecordOriginal :: Text,
    augmentedMutationRecordReplacement :: Text,
    augmentedMutationRecordModule :: Text,
    augmentedMutationRecordLine :: Word,
    augmentedMutationRecordEndLine :: Word,
    augmentedMutationRecordColStart :: Word,
    augmentedMutationRecordColEnd :: Word,
    augmentedMutationRecordSourceFile :: Maybe (Path Rel File),
    augmentedMutationRecordSourceLines :: [Text],
    augmentedMutationRecordMutatedLines :: [Text],
    augmentedMutationRecordContextBefore :: [Text],
    augmentedMutationRecordContextAfter :: [Text],
    -- | Tests whose execution reaches this mutation site, keyed by test suite
    -- name.  The empty string @""@ is used for anonymous\/single-suite setups
    -- (backward-compatible with the old flat list format).
    -- Always present (coverage was collected before writing this file).
    augmentedMutationRecordCoveringTests :: Map.Map Text [TestId],
    -- | Monotonic-clock timeout (in microseconds) to apply to a mutation child
    -- running this mutation.  Derived during the coverage phase as
    -- @max 30_000_000 (10 * sum baselines_of_covering_tests)@.
    augmentedMutationRecordTimeoutMicros :: Word,
    -- | Source name of the enclosing top-level binding, if known.  Carried
    -- through from 'mutRecBinding' so the report can suggest the exact
    -- @{-# ANN \<binding\> ... #-}@ disable annotation.
    augmentedMutationRecordBinding :: Maybe Text,
    -- | Optional mitigation hint, carried through from 'mutRecMitigation'.
    augmentedMutationRecordMitigation :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec AugmentedMutationRecord)

-- | Codec for 'Map Text [TestId]': a JSON object keyed by suite name.
coveringTestsCodec :: JSONCodec (Map.Map Text [TestId])
coveringTestsCodec = codec

instance HasCodec AugmentedMutationRecord where
  codec =
    object "AugmentedMutationRecord" $
      AugmentedMutationRecord
        <$> requiredField' "id" .= augmentedMutationRecordId
        <*> requiredField' "operator" .= augmentedMutationRecordOperator
        <*> requiredField' "original" .= augmentedMutationRecordOriginal
        <*> requiredField' "replacement" .= augmentedMutationRecordReplacement
        <*> requiredField' "module" .= augmentedMutationRecordModule
        <*> requiredField' "line" .= augmentedMutationRecordLine
        <*> optionalFieldWithDefault' "end_line" 0 .= augmentedMutationRecordEndLine
        <*> requiredField' "col_start" .= augmentedMutationRecordColStart
        <*> requiredField' "col_end" .= augmentedMutationRecordColEnd
        <*> optionalFieldWith' "source_file" relFileCodec .= augmentedMutationRecordSourceFile
        <*> optionalFieldWithDefault' "source_lines" [] .= augmentedMutationRecordSourceLines
        <*> optionalFieldWithDefault' "mutated_lines" [] .= augmentedMutationRecordMutatedLines
        <*> optionalFieldWithDefault' "context_before" [] .= augmentedMutationRecordContextBefore
        <*> optionalFieldWithDefault' "context_after" [] .= augmentedMutationRecordContextAfter
        <*> optionalFieldWithDefaultWith' "covering_tests" coveringTestsCodec Map.empty .= augmentedMutationRecordCoveringTests
        <*> requiredField' "timeout_micros" .= augmentedMutationRecordTimeoutMicros
        <*> optionalField' "binding" .= augmentedMutationRecordBinding
        <*> optionalField' "mitigation" .= augmentedMutationRecordMitigation

instance Validity AugmentedMutationRecord

instance GenValid AugmentedMutationRecord where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

-- | A group of augmented mutation records sharing one operator-at-one-location
-- origin.  The runner walks groups concurrently and walks records within a
-- group sequentially; the first failing record in a group skips the
-- remaining records (within-group fail-fast).
newtype AugmentedMutationGroup = AugmentedMutationGroup [AugmentedMutationRecord]
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec AugmentedMutationGroup)

instance Validity AugmentedMutationGroup

instance GenValid AugmentedMutationGroup where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec AugmentedMutationGroup where
  codec = dimapCodec AugmentedMutationGroup (\(AugmentedMutationGroup rs) -> rs) codec

newtype AugmentedManifest = AugmentedManifest [AugmentedMutationGroup]
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec AugmentedManifest)

instance Validity AugmentedManifest

instance GenValid AugmentedManifest where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec AugmentedManifest where
  codec = dimapCodec AugmentedManifest (\(AugmentedManifest gs) -> gs) codec

instance Semigroup AugmentedManifest where
  AugmentedManifest a <> AugmentedManifest b = AugmentedManifest (a <> b)

instance Monoid AugmentedManifest where
  mempty = AugmentedManifest []

-- | Keep only the mutation records whose id is in the given set, dropping any
-- group that ends up empty.  Group order and within-group order are
-- preserved, so the filtered manifest runs the selected mutations in the same
-- order a full run would.
filterAugmentedManifestByIds :: Set.Set MutationId -> AugmentedManifest -> AugmentedManifest
filterAugmentedManifestByIds ids (AugmentedManifest groups) =
  AugmentedManifest
    [ AugmentedMutationGroup kept
    | AugmentedMutationGroup recs <- groups,
      let kept = filter ((`Set.member` ids) . augmentedMutationRecordId) recs,
      not (null kept)
    ]

augmentedManifestRelFile :: Path Rel File
augmentedManifestRelFile = [relfile|manifest-augmented.json|]

-- | 30 seconds, used as the floor for per-mutation monotonic-clock budgets and as
-- the initial value when a record is constructed without baseline timing
-- (i.e. before the coverage phase has annotated it).  The actual budget is
-- @max defaultTimeoutMicros (10 * sum baselines_of_covering_tests)@.
defaultTimeoutMicros :: Word
defaultTimeoutMicros = 30_000_000

-- | Write to @<dir>/manifest-augmented.json@.
writeAugmentedManifestFile :: Path Abs Dir -> AugmentedManifest -> IO ()
writeAugmentedManifestFile dir manifest = do
  ensureDir dir
  LB.writeFile (fromAbsFile (dir </> augmentedManifestRelFile)) (Aeson.encode manifest)

-- | Thrown by 'readAugmentedManifestFile' when the file cannot be decoded.
newtype AugmentedManifestDecodeException
  = AugmentedManifestDecodeException FilePath
  deriving (Show)

instance Exception AugmentedManifestDecodeException

-- | Read from @<dir>/manifest-augmented.json@.
--
-- Reads strictly (via 'SB.readFile' + 'Aeson.decodeStrict') so the file
-- handle is closed before this function returns.  This is a defensive
-- measure: under heavy concurrency the original 'LB.readFile' +
-- 'Aeson.decode' path was a suspected (but unproven) contributor to a
-- non-deterministic 'BlockedIndefinitelyOnMVar' / @<<loop>>@ at the
-- coverage/mutation phase boundary on large projects.
readAugmentedManifestFile :: Path Abs Dir -> IO AugmentedManifest
readAugmentedManifestFile dir = do
  let path = dir </> augmentedManifestRelFile
  result <- Aeson.decodeStrict <$> SB.readFile (fromAbsFile path)
  case result of
    Nothing -> throwIO (AugmentedManifestDecodeException (fromAbsFile path))
    Just m -> pure m

-- | Read from @<dir>/manifest-augmented.json@, returning 'Nothing' if the file
-- does not exist.
readAugmentedManifestFileIfExists :: Path Abs Dir -> IO (Maybe AugmentedManifest)
readAugmentedManifestFileIfExists dir =
  forgivingAbsence (readAugmentedManifestFile dir)

-- | Merge two 'AugmentedManifest's, combining 'covering_tests' maps by
-- mutation id.  Records present only in one manifest are kept as-is.
-- Group structure is preserved: a record in 'new' is matched into the base
-- group whose first record (by id lookup) it shares.  Groups present only in
-- 'new' are appended.
mergeAugmentedManifests :: AugmentedManifest -> AugmentedManifest -> AugmentedManifest
mergeAugmentedManifests (AugmentedManifest base) (AugmentedManifest new) =
  AugmentedManifest (map mergeGroup base ++ newOnlyGroups)
  where
    newRecsById :: Map.Map MutationId AugmentedMutationRecord
    newRecsById =
      Map.fromList
        [ (augmentedMutationRecordId r, r)
        | AugmentedMutationGroup rs <- new,
          r <- rs
        ]
    baseIds :: Map.Map MutationId ()
    baseIds =
      Map.fromList
        [ (augmentedMutationRecordId r, ())
        | AugmentedMutationGroup rs <- base,
          r <- rs
        ]
    newOnlyGroups =
      [ AugmentedMutationGroup keptRecs
      | AugmentedMutationGroup rs <- new,
        let keptRecs =
              filter
                (\r -> Map.notMember (augmentedMutationRecordId r) baseIds)
                rs,
        not (null keptRecs)
      ]
    mergeGroup (AugmentedMutationGroup rs) =
      AugmentedMutationGroup (map mergeRecord rs)
    mergeRecord r =
      case Map.lookup (augmentedMutationRecordId r) newRecsById of
        Nothing -> r
        Just r' ->
          r
            { augmentedMutationRecordCoveringTests =
                Map.unionWith
                  mergeCoveringTests
                  (augmentedMutationRecordCoveringTests r)
                  (augmentedMutationRecordCoveringTests r'),
              -- Take the larger of the two timeouts so a generously-budgeted
              -- suite is not penalised when merged with a stricter one.
              augmentedMutationRecordTimeoutMicros =
                max
                  (augmentedMutationRecordTimeoutMicros r)
                  (augmentedMutationRecordTimeoutMicros r')
            }
    -- Concatenate covering-test lists from two manifests, but drop any
    -- 'TestId' that already appears in the base list.  Treating the lists
    -- as sets makes the merge idempotent: @mergeAugmentedManifests m m@
    -- equals @m@.
    mergeCoveringTests baseTids newTids =
      let baseSet = Set.fromList baseTids
       in baseTids ++ filter (`Set.notMember` baseSet) newTids

-- | Read and union the augmented manifests from a list of per-package coverage
-- directories.  Each directory is a @coverage@-subcommand output holding
-- @augmented/manifest-augmented.json@; unioning them with
-- 'mergeAugmentedManifests' combines the per-suite @covering_tests@ so a
-- mutation covered by a test in any package is recorded — including
-- cross-package coverage (a suite in one package covering another package's
-- mutation).  The @run@ subcommand (per-library report) and the @diff@
-- subcommand both consume the per-package coverage this way.
readAndUnionCoverageDirs :: [Path Abs Dir] -> IO AugmentedManifest
readAndUnionCoverageDirs coverageDirs =
  foldl' mergeAugmentedManifests mempty
    <$> mapM (\dir -> readAugmentedManifestFile (dir </> [reldir|augmented|])) coverageDirs

-- | O(n) lookup by 'MutationId' across every group.
lookupAugmentedMutationRecord :: MutationId -> AugmentedManifest -> Maybe AugmentedMutationRecord
lookupAugmentedMutationRecord mid (AugmentedManifest groups) =
  case [r | AugmentedMutationGroup rs <- groups, r <- rs, augmentedMutationRecordId r == mid] of
    (r : _) -> Just r
    [] -> Nothing

-- | A survived mutation with an optional pointer to the raw child output file.
data SurvivedMutation = SurvivedMutation
  { survivedMutationRecord :: AugmentedMutationRecord,
    -- | Path to the raw child output file, relative to the report directory.
    survivedMutationLogFile :: Maybe (Path Rel File)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec SurvivedMutation)

instance Validity SurvivedMutation

instance GenValid SurvivedMutation where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec SurvivedMutation where
  codec =
    object "SurvivedMutation" $
      SurvivedMutation
        <$> requiredField' "mutation" .= survivedMutationRecord
        <*> optionalFieldWith' "log_file" relFileCodec .= survivedMutationLogFile

-- | A mutation child that exceeded its monotonic-clock timeout and was killed by
-- the parent.  Treated as killed for the overall score (a hung mutation is
-- still a broken mutation), but reported separately for visibility.
data TimedOutMutation = TimedOutMutation
  { timedOutMutationRecord :: AugmentedMutationRecord,
    -- | Monotonic-clock microseconds elapsed before the parent killed the child.
    timedOutMutationElapsedMicros :: Word,
    -- | Path to the raw child output file (the bit produced before the kill),
    -- relative to the report directory.
    timedOutMutationLogFile :: Maybe (Path Rel File)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec TimedOutMutation)

instance Validity TimedOutMutation

instance GenValid TimedOutMutation where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec TimedOutMutation where
  codec =
    object "TimedOutMutation" $
      TimedOutMutation
        <$> requiredField' "mutation" .= timedOutMutationRecord
        <*> requiredField' "elapsed_micros" .= timedOutMutationElapsedMicros
        <*> optionalFieldWith' "log_file" relFileCodec .= timedOutMutationLogFile

-- | A mutation that was not covered by any test (never executed).
newtype UncoveredMutation = UncoveredMutation
  { uncoveredMutationRecord :: AugmentedMutationRecord
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec UncoveredMutation)

instance Validity UncoveredMutation

instance GenValid UncoveredMutation where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec UncoveredMutation where
  codec =
    object "UncoveredMutation" $
      UncoveredMutation
        <$> requiredField' "mutation" .= uncoveredMutationRecord

-- | A mutation that was not tested because an earlier mutation in the same
-- group already failed (survived or was uncovered).  The 'skippedMutationCause'
-- points at the id of that earlier mutation.
data SkippedMutation = SkippedMutation
  { skippedMutationRecord :: AugmentedMutationRecord,
    skippedMutationCause :: MutationId
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec SkippedMutation)

instance Validity SkippedMutation

instance GenValid SkippedMutation where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec SkippedMutation where
  codec =
    object "SkippedMutation" $
      SkippedMutation
        <$> requiredField' "mutation" .= skippedMutationRecord
        <*> requiredField' "cause" .= skippedMutationCause

-- | A control (no-op) mutation that was killed - i.e. the control /failed/.
--
-- A control changes no behaviour, so it must survive.  A killed control means
-- the mutation testing is unsound (a flaky or nondeterministic test suite, or
-- a bug in the harness itself), so it fails the run like a survivor rather than
-- being counted as a real kill.  Carries the optional child output file, like a
-- survivor, so the report can show what the suite did.
data ControlFailedMutation = ControlFailedMutation
  { controlFailedMutationRecord :: AugmentedMutationRecord,
    -- | Path to the raw child output file, relative to the report directory.
    controlFailedMutationLogFile :: Maybe (Path Rel File)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec ControlFailedMutation)

instance Validity ControlFailedMutation

instance GenValid ControlFailedMutation where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec ControlFailedMutation where
  codec =
    object "ControlFailedMutation" $
      ControlFailedMutation
        <$> requiredField' "mutation" .= controlFailedMutationRecord
        <*> optionalFieldWith' "log_file" relFileCodec .= controlFailedMutationLogFile

-- | One mutation's outcome within a group.
data MutationOutcome
  = OutcomeKilled AugmentedMutationRecord
  | OutcomeSurvived SurvivedMutation
  | OutcomeTimedOut TimedOutMutation
  | OutcomeUncovered UncoveredMutation
  | OutcomeSkipped SkippedMutation
  | -- | A control (no-op) mutation that survived, as it must.  The control
    -- /passed/: it confirms the harness correctly reports a non-diff as a
    -- survivor.  Excluded from the killed\/survived score.
    OutcomeControlPassed AugmentedMutationRecord
  | -- | A control (no-op) mutation that was killed - the control /failed/.
    -- Not part of the killed\/survived score, but fails the run; see
    -- 'ControlFailedMutation'.
    OutcomeControlFailed ControlFailedMutation
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec MutationOutcome)

instance Validity MutationOutcome

instance GenValid MutationOutcome where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec MutationOutcome where
  codec =
    object "MutationOutcome" $
      discriminatedUnionCodec
        "outcome"
        ( \case
            OutcomeKilled r -> ("killed", mapToEncoder r killedSubCodec)
            OutcomeSurvived s -> ("survived", mapToEncoder s survivedSubCodec)
            OutcomeTimedOut t -> ("timed_out", mapToEncoder t timedOutSubCodec)
            OutcomeUncovered u -> ("uncovered", mapToEncoder u uncoveredSubCodec)
            OutcomeSkipped sk -> ("skipped", mapToEncoder sk skippedSubCodec)
            OutcomeControlPassed r -> ("control_passed", mapToEncoder r controlPassedSubCodec)
            OutcomeControlFailed cf -> ("control_failed", mapToEncoder cf controlFailedSubCodec)
        )
        ( HashMap.fromList
            [ ("killed", ("OutcomeKilled", mapToDecoder OutcomeKilled killedSubCodec)),
              ("survived", ("OutcomeSurvived", mapToDecoder OutcomeSurvived survivedSubCodec)),
              ("timed_out", ("OutcomeTimedOut", mapToDecoder OutcomeTimedOut timedOutSubCodec)),
              ("uncovered", ("OutcomeUncovered", mapToDecoder OutcomeUncovered uncoveredSubCodec)),
              ("skipped", ("OutcomeSkipped", mapToDecoder OutcomeSkipped skippedSubCodec)),
              ("control_passed", ("OutcomeControlPassed", mapToDecoder OutcomeControlPassed controlPassedSubCodec)),
              ("control_failed", ("OutcomeControlFailed", mapToDecoder OutcomeControlFailed controlFailedSubCodec))
            ]
        )
    where
      killedSubCodec :: JSONObjectCodec AugmentedMutationRecord
      killedSubCodec = requiredField' "mutation"
      survivedSubCodec :: JSONObjectCodec SurvivedMutation
      survivedSubCodec =
        SurvivedMutation
          <$> requiredField' "mutation" .= survivedMutationRecord
          <*> optionalFieldWith' "log_file" relFileCodec .= survivedMutationLogFile
      timedOutSubCodec :: JSONObjectCodec TimedOutMutation
      timedOutSubCodec =
        TimedOutMutation
          <$> requiredField' "mutation" .= timedOutMutationRecord
          <*> requiredField' "elapsed_micros" .= timedOutMutationElapsedMicros
          <*> optionalFieldWith' "log_file" relFileCodec .= timedOutMutationLogFile
      uncoveredSubCodec :: JSONObjectCodec UncoveredMutation
      uncoveredSubCodec = UncoveredMutation <$> requiredField' "mutation" .= uncoveredMutationRecord
      skippedSubCodec :: JSONObjectCodec SkippedMutation
      skippedSubCodec =
        SkippedMutation
          <$> requiredField' "mutation" .= skippedMutationRecord
          <*> requiredField' "cause" .= skippedMutationCause
      controlPassedSubCodec :: JSONObjectCodec AugmentedMutationRecord
      controlPassedSubCodec = requiredField' "mutation"
      controlFailedSubCodec :: JSONObjectCodec ControlFailedMutation
      controlFailedSubCodec =
        ControlFailedMutation
          <$> requiredField' "mutation" .= controlFailedMutationRecord
          <*> optionalFieldWith' "log_file" relFileCodec .= controlFailedMutationLogFile

-- | All outcomes for the mutations of one group, in their original order.
newtype MutationGroupReport = MutationGroupReport
  { mutationGroupReportOutcomes :: [MutationOutcome]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec MutationGroupReport)

instance HasCodec MutationGroupReport where
  codec =
    dimapCodec MutationGroupReport mutationGroupReportOutcomes codec

instance Validity MutationGroupReport

instance GenValid MutationGroupReport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

-- | The score for the normal (non-control) mutations of a run.
--
-- 'mutationTallyKilled' includes timed-out mutations (a hung mutation is
-- treated as killed for scoring).  'mutationTallyTimedOut' is the count of
-- those specifically.  'mutationTallySkipped' counts mutations that were not
-- tested because an earlier mutation in the same group already failed.
data MutationTally = MutationTally
  { mutationTallyKilled :: Word,
    mutationTallySurvived :: Word,
    mutationTallyTimedOut :: Word,
    mutationTallyUncovered :: Word,
    mutationTallySkipped :: Word
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec MutationTally)

instance Validity MutationTally

instance GenValid MutationTally where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec MutationTally where
  codec =
    object "MutationTally" $
      MutationTally
        <$> requiredField' "killed" .= mutationTallyKilled
        <*> requiredField' "survived" .= mutationTallySurvived
        <*> requiredField' "timed_out" .= mutationTallyTimedOut
        <*> requiredField' "uncovered" .= mutationTallyUncovered
        <*> requiredField' "skipped" .= mutationTallySkipped

-- | The score for the control (no-op) mutations of a run.  Controls are
-- excluded from 'MutationTally'; a passed control survived as it must, a failed
-- control was killed (the mutation testing is unsound - a flaky\/nondeterministic
-- suite or a harness bug - not a real kill, and it fails the run).
data ControlTally = ControlTally
  { controlTallyPassed :: Word,
    controlTallyFailed :: Word
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec ControlTally)

instance Validity ControlTally

instance GenValid ControlTally where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec ControlTally where
  codec =
    object "ControlTally" $
      ControlTally
        <$> requiredField' "passed" .= controlTallyPassed
        <*> requiredField' "failed" .= controlTallyFailed

-- | Full JSON report written by the parent mutation process, in three parts:
-- the normal-mutation score ('mutationRunReportMutations'), the control-mutation
-- score ('mutationRunReportControls'), and the per-mutation detail
-- ('mutationRunReportGroups'), which mirrors the manifest's group structure.
data MutationRunReport = MutationRunReport
  { mutationRunReportMutations :: MutationTally,
    mutationRunReportControls :: ControlTally,
    mutationRunReportGroups :: [MutationGroupReport]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec MutationRunReport)

instance Validity MutationRunReport

instance GenValid MutationRunReport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec MutationRunReport where
  codec =
    object "MutationRunReport" $
      MutationRunReport
        <$> requiredField' "mutations" .= mutationRunReportMutations
        <*> requiredField' "controls" .= mutationRunReportControls
        <*> requiredField' "groups" .= mutationRunReportGroups

mutationRunReportRelFile :: Path Rel File
mutationRunReportRelFile = [relfile|report.json|]

-- | Write @report.json@ to the given directory.
writeMutationRunReport :: Path Abs Dir -> MutationRunReport -> IO ()
writeMutationRunReport dir report = do
  ensureDir dir
  LB.writeFile (fromAbsFile (dir </> mutationRunReportRelFile)) (Aeson.encode report)

-- | Thrown by 'readMutationRunReport' when @report.json@ cannot be
-- decoded.  Mirrors 'AugmentedManifestDecodeException'.
newtype MutationRunReportDecodeException
  = MutationRunReportDecodeException FilePath
  deriving (Show)

instance Exception MutationRunReportDecodeException

-- | Read @report.json@ from the given directory.
--
-- Reads strictly (via 'SB.readFile' + 'Aeson.decodeStrict') so the file
-- handle is closed before this function returns.  Throws
-- 'MutationRunReportDecodeException' on a decode failure rather than
-- silently producing a 'Maybe', so a caller that depends on the report
-- shape (the @assert-score@ subcommand) fails with an attributable
-- error.
readMutationRunReport :: Path Abs Dir -> IO MutationRunReport
readMutationRunReport dir = do
  let path = dir </> mutationRunReportRelFile
  result <- Aeson.decodeStrict <$> SB.readFile (fromAbsFile path)
  case result of
    Nothing -> throwIO (MutationRunReportDecodeException (fromAbsFile path))
    Just m -> pure m

-- | Convert a 'MutationRecord' with coverage data to an 'AugmentedMutationRecord'.
-- Records with 'mutRecCoveringTests' = 'Nothing' are dropped.
fromMutationRecord :: MutationRecord -> Maybe AugmentedMutationRecord
fromMutationRecord MutationRecord {mutRecId, mutRecOperator, mutRecOriginal, mutRecReplacement, mutRecModule, mutRecLine, mutRecEndLine, mutRecColStart, mutRecColEnd, mutRecSourceFile, mutRecSourceLines, mutRecMutatedLines, mutRecContextBefore, mutRecContextAfter, mutRecCoveringTests, mutRecBinding, mutRecMitigation} =
  case mutRecCoveringTests of
    Nothing -> Nothing
    Just ts ->
      Just
        AugmentedMutationRecord
          { augmentedMutationRecordId = mutRecId,
            augmentedMutationRecordOperator = mutRecOperator,
            augmentedMutationRecordOriginal = mutRecOriginal,
            augmentedMutationRecordReplacement = mutRecReplacement,
            augmentedMutationRecordModule = mutRecModule,
            augmentedMutationRecordLine = mutRecLine,
            augmentedMutationRecordEndLine = mutRecEndLine,
            augmentedMutationRecordColStart = mutRecColStart,
            augmentedMutationRecordColEnd = mutRecColEnd,
            augmentedMutationRecordSourceFile = mutRecSourceFile,
            augmentedMutationRecordSourceLines = mutRecSourceLines,
            augmentedMutationRecordMutatedLines = mutRecMutatedLines,
            augmentedMutationRecordContextBefore = mutRecContextBefore,
            augmentedMutationRecordContextAfter = mutRecContextAfter,
            augmentedMutationRecordCoveringTests = ts,
            -- Filled in later by 'annotateRecord' in runCoverageMode; this
            -- code path constructs the record from a raw MutationRecord
            -- that has no baseline info, so we use the floor as a safe
            -- initial value.
            augmentedMutationRecordTimeoutMicros = defaultTimeoutMicros,
            augmentedMutationRecordBinding = mutRecBinding,
            augmentedMutationRecordMitigation = mutRecMitigation
          }

-- | A mutation that is about to be tested, used as the progress log event.
newtype MutationProgressEvent = MutationProgressEvent
  { mutationProgressRecord :: AugmentedMutationRecord
  }
  deriving stock (Show, Eq, Generic)

instance Validity MutationProgressEvent

instance GenValid MutationProgressEvent where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
