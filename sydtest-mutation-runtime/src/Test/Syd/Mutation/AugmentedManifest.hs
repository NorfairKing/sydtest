{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.Mutation.AugmentedManifest
  ( AugmentedMutationRecord (..),
    AugmentedManifest (..),
    mergeAugmentedManifests,
    writeAugmentedManifestFile,
    readAugmentedManifestFile,
    readAugmentedManifestFileIfExists,
    lookupAugmentedMutationRecord,
    fromMutationRecord,
    SurvivedMutation (..),
    UncoveredMutation (..),
    MutationRunReport (..),
    writeMutationRunReport,
    renderMutationRunReport,
    MutationProgressEvent (..),
    renderMutationProgressEvent,
    formatMutationLog,
  )
where

import Autodocodec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.GenValidity.Map ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Myers.Diff (PolyDiff (..), getGroupedDiff)
import Path
import Path.IO (doesFileExist, ensureDir)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Test.Syd.Mutation.Manifest (MutationRecord (..), relFileCodec)
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId)
import Text.Colour (Chunk, chunk, cyan, fore, green, red, yellow)

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
    augmentedMutationRecordSourceLines :: [Text],
    augmentedMutationRecordMutatedLines :: [Text],
    augmentedMutationRecordContextBefore :: [Text],
    augmentedMutationRecordContextAfter :: [Text],
    -- | Tests whose execution reaches this mutation site, keyed by test suite
    -- name.  The empty string @""@ is used for anonymous\/single-suite setups
    -- (backward-compatible with the old flat list format).
    -- Always present (coverage was collected before writing this file).
    augmentedMutationRecordCoveringTests :: Map.Map Text [TestId]
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
        <*> requiredField' "col_start" .= augmentedMutationRecordColStart
        <*> requiredField' "col_end" .= augmentedMutationRecordColEnd
        <*> optionalFieldWith' "source_file" relFileCodec .= augmentedMutationRecordSourceFile
        <*> optionalFieldWithDefault' "source_lines" [] .= augmentedMutationRecordSourceLines
        <*> optionalFieldWithDefault' "mutated_lines" [] .= augmentedMutationRecordMutatedLines
        <*> optionalFieldWithDefault' "context_before" [] .= augmentedMutationRecordContextBefore
        <*> optionalFieldWithDefault' "context_after" [] .= augmentedMutationRecordContextAfter
        <*> optionalFieldWithDefaultWith' "covering_tests" coveringTestsCodec Map.empty .= augmentedMutationRecordCoveringTests

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

-- | Read from @<dir>/manifest-augmented.json@, returning 'Nothing' if the file
-- does not exist.
readAugmentedManifestFileIfExists :: Path Abs Dir -> IO (Maybe AugmentedManifest)
readAugmentedManifestFileIfExists dir = do
  let path = dir </> augmentedManifestRelFile
  exists <- doesFileExist path
  if exists
    then Just <$> readAugmentedManifestFile dir
    else pure Nothing

-- | Merge two 'AugmentedManifest's, combining 'covering_tests' maps by
-- mutation id.  Records present only in one manifest are kept as-is.
mergeAugmentedManifests :: AugmentedManifest -> AugmentedManifest -> AugmentedManifest
mergeAugmentedManifests (AugmentedManifest base) (AugmentedManifest new) =
  AugmentedManifest (map mergeRecord base ++ newOnly)
  where
    newById = Map.fromList [(augmentedMutationRecordId r, r) | r <- new]
    baseIds = Map.fromList [(augmentedMutationRecordId r, ()) | r <- base]
    newOnly = filter (\r -> Map.notMember (augmentedMutationRecordId r) baseIds) new
    mergeRecord r =
      case Map.lookup (augmentedMutationRecordId r) newById of
        Nothing -> r
        Just r' ->
          r
            { augmentedMutationRecordCoveringTests =
                Map.unionWith
                  (++)
                  (augmentedMutationRecordCoveringTests r)
                  (augmentedMutationRecordCoveringTests r')
            }

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
fromMutationRecord MutationRecord {mutRecId, mutRecOperator, mutRecOriginal, mutRecReplacement, mutRecModule, mutRecLine, mutRecColStart, mutRecColEnd, mutRecSourceFile, mutRecSourceLines, mutRecMutatedLines, mutRecContextBefore, mutRecContextAfter, mutRecCoveringTests} =
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
            augmentedMutationRecordSourceLines = mutRecSourceLines,
            augmentedMutationRecordMutatedLines = mutRecMutatedLines,
            augmentedMutationRecordContextBefore = mutRecContextBefore,
            augmentedMutationRecordContextAfter = mutRecContextAfter,
            augmentedMutationRecordCoveringTests = ts
          }

-- | Render a 'MutationRunReport' as coloured lines.
renderMutationRunReport :: MutationRunReport -> [[Chunk]]
renderMutationRunReport MutationRunReport {mutationRunReportKilled, mutationRunReportSurvived, mutationRunReportUncovered, mutationRunReportSurvivors} =
  [ [chunk "Killed: ", fore green (chunk (T.pack (show mutationRunReportKilled)))],
    [chunk "Survived: ", fore red (chunk (T.pack (show mutationRunReportSurvived)))],
    [chunk "Uncovered: ", fore yellow (chunk (T.pack (show mutationRunReportUncovered)))]
  ]
    ++ if null mutationRunReportSurvivors
      then []
      else
        [[], [chunk "Surviving mutations:"]]
          ++ concatMap renderSurvivor mutationRunReportSurvivors
  where
    renderSurvivor sm =
      let rec = survivedMutationRecord sm
          mid = augmentedMutationRecordId rec
       in [] : formatMutationLog mid rec

-- | A mutation that is about to be tested, used as the progress log event.
newtype MutationProgressEvent = MutationProgressEvent
  { mutationProgressRecord :: AugmentedMutationRecord
  }

-- | Render a progress event for stderr: @Testing mutation \<op\> at \<srcloc\>@ followed by the diff.
renderMutationProgressEvent :: MutationProgressEvent -> [[Chunk]]
renderMutationProgressEvent (MutationProgressEvent rec) =
  let logLines = formatMutationLog (augmentedMutationRecordId rec) rec
   in case logLines of
        [] -> [[chunk "Testing mutation"]]
        (firstLine : rest) -> (chunk "Testing mutation " : firstLine) : rest

-- | Format a survived mutation as @\<op\> at \<srcloc\>@ followed by the diff,
-- for use in the survivors section of the final report.
formatMutationLog :: MutationId -> AugmentedMutationRecord -> [[Chunk]]
formatMutationLog (MutationId parts) AugmentedMutationRecord {augmentedMutationRecordOperator, augmentedMutationRecordOriginal, augmentedMutationRecordReplacement, augmentedMutationRecordSourceLines, augmentedMutationRecordMutatedLines, augmentedMutationRecordSourceFile, augmentedMutationRecordLine, augmentedMutationRecordContextBefore, augmentedMutationRecordContextAfter} =
  case parts of
    (modName : _op : lineStr : colStartStr : colEndStr : _) ->
      let filePath = case augmentedMutationRecordSourceFile of
            Just p -> fromRelFile p
            Nothing -> moduleToFilePath modName
          headerText = T.pack $ T.unpack augmentedMutationRecordOperator ++ " at " ++ filePath ++ ":" ++ lineStr ++ ":" ++ colStartStr ++ "-" ++ colEndStr
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

-- | Render a unified diff of the source change.
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
   in [fore cyan (chunk hunkHeader)] : concatMap renderGroup groups
  where
    renderGroup = \case
      Both ls _ -> map (\l -> [chunk (T.cons ' ' l)]) ls
      First ls -> map (\l -> [fore red (chunk (T.cons '-' l))]) ls
      Second ls -> map (\l -> [fore green (chunk (T.cons '+' l))]) ls
