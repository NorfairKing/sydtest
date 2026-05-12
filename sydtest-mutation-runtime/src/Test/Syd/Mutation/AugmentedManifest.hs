{-# LANGUAGE DeriveGeneric #-}
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

import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode, object, withArray, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Path
import Path.IO (ensureDir)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Syd.Mutation.Manifest (MutationRecord (..))
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId, parseTestIdFilterArg, renderTestId)

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
  deriving (Show, Eq, Generic)

instance ToJSON AugmentedMutationRecord where
  toJSON AugmentedMutationRecord {augmentedMutationRecordId = MutationId parts, augmentedMutationRecordOperator, augmentedMutationRecordOriginal, augmentedMutationRecordReplacement, augmentedMutationRecordModule, augmentedMutationRecordLine, augmentedMutationRecordColStart, augmentedMutationRecordColEnd, augmentedMutationRecordSourceFile, augmentedMutationRecordSourceLine, augmentedMutationRecordMutatedLine, augmentedMutationRecordContextBefore, augmentedMutationRecordContextAfter, augmentedMutationRecordCoveringTests} =
    object
      [ "id" .= parts,
        "operator" .= augmentedMutationRecordOperator,
        "original" .= augmentedMutationRecordOriginal,
        "replacement" .= augmentedMutationRecordReplacement,
        "module" .= augmentedMutationRecordModule,
        "line" .= augmentedMutationRecordLine,
        "col_start" .= augmentedMutationRecordColStart,
        "col_end" .= augmentedMutationRecordColEnd,
        "source_file" .= fmap fromRelFile augmentedMutationRecordSourceFile,
        "source_line" .= augmentedMutationRecordSourceLine,
        "mutated_line" .= augmentedMutationRecordMutatedLine,
        "context_before" .= augmentedMutationRecordContextBefore,
        "context_after" .= augmentedMutationRecordContextAfter,
        "covering_tests" .= map renderTestId augmentedMutationRecordCoveringTests
      ]

instance FromJSON AugmentedMutationRecord where
  parseJSON = withObject "AugmentedMutationRecord" $ \o -> do
    mid <- MutationId <$> o .: "id"
    op <- o .: "operator"
    orig <- o .: "original"
    repl <- o .: "replacement"
    modName <- o .: "module"
    line <- o .: "line"
    colStart <- o .: "col_start"
    colEnd <- o .: "col_end"
    mSrcFileStr <- o .:? "source_file"
    srcFile <- case mSrcFileStr of
      Nothing -> pure Nothing
      Just s -> case parseRelFile s of
        Nothing -> pure Nothing
        Just p -> pure (Just p)
    srcLine <- o .:? "source_line"
    mutatedLine <- o .:? "mutated_line"
    ctxBefore <- o .:? "context_before" .!= []
    ctxAfter <- o .:? "context_after" .!= []
    coveringRaw <- o .:? "covering_tests" .!= []
    let coveringTests = mapMaybe parseTestIdFilterArg (coveringRaw :: [Text])
    pure
      AugmentedMutationRecord
        { augmentedMutationRecordId = mid,
          augmentedMutationRecordOperator = op,
          augmentedMutationRecordOriginal = orig,
          augmentedMutationRecordReplacement = repl,
          augmentedMutationRecordModule = modName,
          augmentedMutationRecordLine = line,
          augmentedMutationRecordColStart = colStart,
          augmentedMutationRecordColEnd = colEnd,
          augmentedMutationRecordSourceFile = srcFile,
          augmentedMutationRecordSourceLine = srcLine,
          augmentedMutationRecordMutatedLine = mutatedLine,
          augmentedMutationRecordContextBefore = ctxBefore,
          augmentedMutationRecordContextAfter = ctxAfter,
          augmentedMutationRecordCoveringTests = coveringTests
        }

instance Validity AugmentedMutationRecord where
  validate = trivialValidation

instance GenValid AugmentedMutationRecord where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

newtype AugmentedManifest = AugmentedManifest [AugmentedMutationRecord]
  deriving (Show, Eq)

instance Semigroup AugmentedManifest where
  AugmentedManifest a <> AugmentedManifest b = AugmentedManifest (a <> b)

instance Monoid AugmentedManifest where
  mempty = AugmentedManifest []

instance ToJSON AugmentedManifest where
  toJSON (AugmentedManifest records) = toJSON records

instance FromJSON AugmentedManifest where
  parseJSON = withArray "AugmentedManifest" $ \arr ->
    AugmentedManifest <$> mapM parseJSON (foldr (:) [] arr)

augmentedManifestRelFile :: Path Rel File
augmentedManifestRelFile = [relfile|manifest-augmented.json|]

-- | Write to @<dir>/manifest-augmented.json@.
writeAugmentedManifestFile :: Path Abs Dir -> AugmentedManifest -> IO ()
writeAugmentedManifestFile dir manifest = do
  ensureDir dir
  LB.writeFile (fromAbsFile (dir </> augmentedManifestRelFile)) (encode manifest)

-- | Read from @<dir>/manifest-augmented.json@.
readAugmentedManifestFile :: Path Abs Dir -> IO AugmentedManifest
readAugmentedManifestFile dir = do
  let path = dir </> augmentedManifestRelFile
  result <- decode <$> LB.readFile (fromAbsFile path)
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
  deriving (Show, Eq)

instance ToJSON SurvivedMutation where
  toJSON SurvivedMutation {survivedMutationRecord, survivedMutationLogFile} =
    object
      [ "mutation" .= survivedMutationRecord,
        "log_file" .= fromRelFile survivedMutationLogFile
      ]

-- | A mutation that was not covered by any test (never executed).
newtype UncoveredMutation = UncoveredMutation
  { uncoveredMutationRecord :: AugmentedMutationRecord
  }
  deriving (Show, Eq)

instance ToJSON UncoveredMutation where
  toJSON UncoveredMutation {uncoveredMutationRecord} =
    object
      [ "mutation" .= uncoveredMutationRecord
      ]

-- | Full JSON report written by the parent mutation process.
data MutationRunReport = MutationRunReport
  { mutationRunReportKilled :: Int,
    mutationRunReportSurvived :: Int,
    mutationRunReportUncovered :: Int,
    mutationRunReportSurvivors :: [SurvivedMutation],
    mutationRunReportUncoveredMutations :: [UncoveredMutation]
  }
  deriving (Show, Eq)

instance ToJSON MutationRunReport where
  toJSON MutationRunReport {mutationRunReportKilled, mutationRunReportSurvived, mutationRunReportUncovered, mutationRunReportSurvivors, mutationRunReportUncoveredMutations} =
    object
      [ "killed" .= mutationRunReportKilled,
        "survived" .= mutationRunReportSurvived,
        "uncovered" .= mutationRunReportUncovered,
        "survivors" .= mutationRunReportSurvivors,
        "uncovered_mutations" .= mutationRunReportUncoveredMutations
      ]

mutationRunReportRelFile :: Path Rel File
mutationRunReportRelFile = [relfile|report.json|]

-- | Write @report.json@ to the given directory.
writeMutationRunReport :: Path Abs Dir -> MutationRunReport -> IO ()
writeMutationRunReport dir report = do
  ensureDir dir
  LB.writeFile (fromAbsFile (dir </> mutationRunReportRelFile)) (encode report)

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
