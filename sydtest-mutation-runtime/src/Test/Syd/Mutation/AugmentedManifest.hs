{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.AugmentedManifest
  ( AugmentedMutationRecord (..),
    AugmentedManifest (..),
    writeAugmentedManifestFile,
    readAugmentedManifestFile,
    lookupAugmentedMutationRecord,
    fromMutationRecord,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode, object, withArray, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Path
import Path.IO (ensureDir)
import System.IO (hPutStrLn, stderr)
import Test.Syd.Mutation.Manifest (MutationRecord (..))
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId, parseTestIdFilterArg, renderTestId)

-- | A mutation record augmented with coverage data.
-- Unlike 'MutationRecord', covering tests are always present (never Nothing).
data AugmentedMutationRecord = AugmentedMutationRecord
  { augmentedMutationRecordId :: MutationId,
    augmentedMutationRecordOperator :: String,
    augmentedMutationRecordOriginal :: String,
    augmentedMutationRecordReplacement :: String,
    augmentedMutationRecordSourceFile :: Maybe (Path Rel File),
    augmentedMutationRecordSourceLine :: Maybe Text,
    augmentedMutationRecordMutatedLine :: Maybe Text,
    augmentedMutationRecordContextBefore :: [Text],
    augmentedMutationRecordContextAfter :: [Text],
    -- | Tests whose execution reaches this mutation site.
    -- Always present (coverage was collected before writing this file).
    augmentedMutationRecordCoveringTests :: [TestId]
  }
  deriving (Show, Eq)

instance ToJSON AugmentedMutationRecord where
  toJSON AugmentedMutationRecord {augmentedMutationRecordId = MutationId parts, augmentedMutationRecordOperator, augmentedMutationRecordOriginal, augmentedMutationRecordReplacement, augmentedMutationRecordSourceFile, augmentedMutationRecordSourceLine, augmentedMutationRecordMutatedLine, augmentedMutationRecordContextBefore, augmentedMutationRecordContextAfter, augmentedMutationRecordCoveringTests} =
    object
      [ "id" .= parts,
        "operator" .= augmentedMutationRecordOperator,
        "original" .= augmentedMutationRecordOriginal,
        "replacement" .= augmentedMutationRecordReplacement,
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
          augmentedMutationRecordSourceFile = srcFile,
          augmentedMutationRecordSourceLine = srcLine,
          augmentedMutationRecordMutatedLine = mutatedLine,
          augmentedMutationRecordContextBefore = ctxBefore,
          augmentedMutationRecordContextAfter = ctxAfter,
          augmentedMutationRecordCoveringTests = coveringTests
        }

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

augmentedManifestFileName :: String
augmentedManifestFileName = "manifest-augmented.json"

augmentedManifestRelFile :: Path Rel File
augmentedManifestRelFile =
  case parseRelFile augmentedManifestFileName of
    Just p -> p
    Nothing -> error "augmentedManifestFileName: invalid filename"

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
      pure mempty
    Just m -> pure m

-- | O(n) lookup by 'MutationId'.
lookupAugmentedMutationRecord :: MutationId -> AugmentedManifest -> Maybe AugmentedMutationRecord
lookupAugmentedMutationRecord mid (AugmentedManifest records) =
  case filter (\r -> augmentedMutationRecordId r == mid) records of
    (r : _) -> Just r
    [] -> Nothing

-- | Convert a 'MutationRecord' with coverage data to an 'AugmentedMutationRecord'.
-- Records with 'mutRecCoveringTests' = 'Nothing' are dropped.
fromMutationRecord :: MutationRecord -> Maybe AugmentedMutationRecord
fromMutationRecord MutationRecord {mutRecId, mutRecOperator, mutRecOriginal, mutRecReplacement, mutRecSourceFile, mutRecSourceLine, mutRecMutatedLine, mutRecContextBefore, mutRecContextAfter, mutRecCoveringTests} =
  case mutRecCoveringTests of
    Nothing -> Nothing
    Just ts ->
      Just
        AugmentedMutationRecord
          { augmentedMutationRecordId = mutRecId,
            augmentedMutationRecordOperator = mutRecOperator,
            augmentedMutationRecordOriginal = mutRecOriginal,
            augmentedMutationRecordReplacement = mutRecReplacement,
            augmentedMutationRecordSourceFile = mutRecSourceFile,
            augmentedMutationRecordSourceLine = mutRecSourceLine,
            augmentedMutationRecordMutatedLine = mutRecMutatedLine,
            augmentedMutationRecordContextBefore = mutRecContextBefore,
            augmentedMutationRecordContextAfter = mutRecContextAfter,
            augmentedMutationRecordCoveringTests = ts
          }
