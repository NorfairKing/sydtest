{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.Mutation.AugmentedManifest
  ( AugmentedMutationRecord (..),
    AugmentedManifest (..),
    writeAugmentedManifestFile,
    readAugmentedManifestFile,
    lookupAugmentedMutationRecord,
    fromMutationRecord,
    SurvivedMutation (..),
    UncoveredMutation (..),
    MutationRunReport (..),
    writeMutationRunReport,
  )
where

import Autodocodec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.Text (Text)
import GHC.Generics (Generic)
import Path
import Path.IO (ensureDir)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
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
    augmentedMutationRecordColStart :: Word,
    augmentedMutationRecordColEnd :: Word,
    augmentedMutationRecordSourceFile :: Maybe (Path Rel File),
    augmentedMutationRecordSourceLine :: Maybe Text,
    augmentedMutationRecordMutatedLine :: Maybe Text,
    augmentedMutationRecordContextBefore :: [Text],
    augmentedMutationRecordContextAfter :: [Text],
    -- | Tests whose execution reaches this mutation site.
    -- Always present (coverage was collected before writing this file).
    augmentedMutationRecordCoveringTests :: [TestId]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec AugmentedMutationRecord)

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
        <*> requiredField' "col_start" .= augmentedMutationRecordColStart
        <*> requiredField' "col_end" .= augmentedMutationRecordColEnd
        <*> optionalFieldWith' "source_file" relFileCodec .= augmentedMutationRecordSourceFile
        <*> optionalField' "source_line" .= augmentedMutationRecordSourceLine
        <*> optionalField' "mutated_line" .= augmentedMutationRecordMutatedLine
        <*> optionalFieldWithDefault' "context_before" [] .= augmentedMutationRecordContextBefore
        <*> optionalFieldWithDefault' "context_after" [] .= augmentedMutationRecordContextAfter
        <*> optionalFieldWithDefault' "covering_tests" [] .= augmentedMutationRecordCoveringTests

instance Validity AugmentedMutationRecord where
  validate = trivialValidation

instance GenValid AugmentedMutationRecord where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

newtype AugmentedManifest = AugmentedManifest [AugmentedMutationRecord]
  deriving stock (Show, Eq)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec AugmentedManifest)

instance HasCodec AugmentedManifest where
  codec = dimapCodec AugmentedManifest (\(AugmentedManifest rs) -> rs) codec

instance Semigroup AugmentedManifest where
  AugmentedManifest a <> AugmentedManifest b = AugmentedManifest (a <> b)

instance Monoid AugmentedManifest where
  mempty = AugmentedManifest []

augmentedManifestRelFile :: Path Rel File
augmentedManifestRelFile = [relfile|manifest-augmented.json|]

-- | Write to @<dir>/manifest-augmented.json@.
writeAugmentedManifestFile :: Path Abs Dir -> AugmentedManifest -> IO ()
writeAugmentedManifestFile dir manifest = do
  ensureDir dir
  LB.writeFile (fromAbsFile (dir </> augmentedManifestRelFile)) (Aeson.encode manifest)

-- | Read from @<dir>/manifest-augmented.json@.
readAugmentedManifestFile :: Path Abs Dir -> IO AugmentedManifest
readAugmentedManifestFile dir = do
  let path = dir </> augmentedManifestRelFile
  result <- Aeson.decode <$> LB.readFile (fromAbsFile path)
  case result of
    Nothing -> do
      hPutStrLn stderr $ "mutation: failed to decode augmented manifest " ++ fromAbsFile path
      exitFailure
    Just m -> pure m

-- | O(n) lookup by 'MutationId'.
lookupAugmentedMutationRecord :: MutationId -> AugmentedManifest -> Maybe AugmentedMutationRecord
lookupAugmentedMutationRecord mid (AugmentedManifest records) =
  case filter (\r -> augmentedMutationRecordId r == mid) records of
    (r : _) -> Just r
    [] -> Nothing

-- | A survived mutation with a pointer to the raw child output file.
data SurvivedMutation = SurvivedMutation
  { survivedMutationRecord :: AugmentedMutationRecord,
    -- | Path to the raw child output file, relative to the report directory.
    survivedMutationLogFile :: Path Rel File
  }
  deriving stock (Show, Eq)
  deriving (Aeson.ToJSON) via (Autodocodec SurvivedMutation)

instance HasCodec SurvivedMutation where
  codec =
    object "SurvivedMutation" $
      SurvivedMutation
        <$> requiredField' "mutation" .= survivedMutationRecord
        <*> requiredFieldWith' "log_file" relFileCodec .= survivedMutationLogFile

-- | A mutation that was not covered by any test (never executed).
newtype UncoveredMutation = UncoveredMutation
  { uncoveredMutationRecord :: AugmentedMutationRecord
  }
  deriving stock (Show, Eq)
  deriving (Aeson.ToJSON) via (Autodocodec UncoveredMutation)

instance HasCodec UncoveredMutation where
  codec =
    object "UncoveredMutation" $
      UncoveredMutation
        <$> requiredField' "mutation" .= uncoveredMutationRecord

-- | Full JSON report written by the parent mutation process.
data MutationRunReport = MutationRunReport
  { mutationRunReportKilled :: Int,
    mutationRunReportSurvived :: Int,
    mutationRunReportUncovered :: Int,
    mutationRunReportSurvivors :: [SurvivedMutation],
    mutationRunReportUncoveredMutations :: [UncoveredMutation]
  }
  deriving stock (Show, Eq)
  deriving (Aeson.ToJSON) via (Autodocodec MutationRunReport)

instance HasCodec MutationRunReport where
  codec =
    object "MutationRunReport" $
      MutationRunReport
        <$> requiredField' "killed" .= mutationRunReportKilled
        <*> requiredField' "survived" .= mutationRunReportSurvived
        <*> requiredField' "uncovered" .= mutationRunReportUncovered
        <*> requiredField' "survivors" .= mutationRunReportSurvivors
        <*> requiredField' "uncovered_mutations" .= mutationRunReportUncoveredMutations

mutationRunReportRelFile :: Path Rel File
mutationRunReportRelFile = [relfile|report.json|]

-- | Write @report.json@ to the given directory.
writeMutationRunReport :: Path Abs Dir -> MutationRunReport -> IO ()
writeMutationRunReport dir report = do
  ensureDir dir
  LB.writeFile (fromAbsFile (dir </> mutationRunReportRelFile)) (Aeson.encode report)

-- | Convert a 'MutationRecord' with coverage data to an 'AugmentedMutationRecord'.
-- Records with 'mutRecCoveringTests' = 'Nothing' are dropped.
fromMutationRecord :: MutationRecord -> Maybe AugmentedMutationRecord
fromMutationRecord MutationRecord {mutRecId, mutRecOperator, mutRecOriginal, mutRecReplacement, mutRecModule, mutRecLine, mutRecColStart, mutRecColEnd, mutRecSourceFile, mutRecSourceLine, mutRecMutatedLine, mutRecContextBefore, mutRecContextAfter, mutRecCoveringTests} =
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
            augmentedMutationRecordColStart = mutRecColStart,
            augmentedMutationRecordColEnd = mutRecColEnd,
            augmentedMutationRecordSourceFile = mutRecSourceFile,
            augmentedMutationRecordSourceLine = mutRecSourceLine,
            augmentedMutationRecordMutatedLine = mutRecMutatedLine,
            augmentedMutationRecordContextBefore = mutRecContextBefore,
            augmentedMutationRecordContextAfter = mutRecContextAfter,
            augmentedMutationRecordCoveringTests = ts
          }
