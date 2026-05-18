{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Mutation.Manifest
  ( MutationRecord (..),
    MutationGroup (..),
    MutationManifest (..),
    readManifestFile,
    readManifestDir,
    writeManifestFile,
    readCoverageDir,
    writeCoverageFile,
    relFileCodec,
    MutationAddedEvent (..),
  )
where

import Autodocodec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Path
import Path.IO (ensureDir, listDirRel)
import System.IO (hPutStrLn, stderr)
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId (..))

-- | One discovered mutation site, as recorded by the plugin.
data MutationRecord = MutationRecord
  { mutRecId :: MutationId,
    mutRecOperator :: Text,
    mutRecOriginal :: Text,
    mutRecReplacement :: Text,
    -- | Haskell module name containing the mutation site (e.g. @"Foo.Bar"@).
    mutRecModule :: Text,
    -- | 1-based source line number where the mutated expression starts.
    mutRecLine :: Word,
    -- | 1-based source line number where the mutated expression ends.
    -- For single-line spans this equals 'mutRecLine'.
    mutRecEndLine :: Word,
    -- | 1-based start column of the mutated expression on 'mutRecLine'.
    mutRecColStart :: Word,
    -- | 1-based end column of the mutated expression on 'mutRecEndLine'
    -- (exclusive — one past the last character). When 'mutRecEndLine' differs
    -- from 'mutRecLine', @col_start@ and @col_end@ are independent column
    -- numbers on different lines; they do not form a contiguous range and
    -- @col_end@ can be smaller than @col_start@.
    mutRecColEnd :: Word,
    -- | Source file path relative to the project root, as reported by GHC.
    mutRecSourceFile :: Maybe (Path Rel File),
    -- | Source lines of the mutated expression (from the actual source file).
    mutRecSourceLines :: [Text],
    -- | Source lines after applying the mutation (computed by the plugin).
    mutRecMutatedLines :: [Text],
    -- | Up to 3 source lines immediately before the mutated line.
    mutRecContextBefore :: [Text],
    -- | Up to 3 source lines immediately after the mutated line.
    mutRecContextAfter :: [Text],
    -- | Tests whose execution reaches this mutation site, keyed by test suite
    -- name.  The empty string @""@ is used for anonymous\/single-suite setups.
    -- 'Nothing' means coverage has not been collected yet.
    mutRecCoveringTests :: Maybe (Map.Map Text [TestId])
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec MutationRecord)

instance Validity MutationRecord

instance GenValid MutationRecord where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

-- | Codec for 'Map Text [TestId]': a JSON object keyed by suite name.
coveringTestsMapCodec :: JSONCodec (Map.Map Text [TestId])
coveringTestsMapCodec = codec

instance HasCodec MutationRecord where
  codec =
    object "MutationRecord" $
      MutationRecord
        <$> requiredField' "id" .= mutRecId
        <*> requiredField' "operator" .= mutRecOperator
        <*> requiredField' "original" .= mutRecOriginal
        <*> requiredField' "replacement" .= mutRecReplacement
        <*> requiredField' "module" .= mutRecModule
        <*> requiredField' "line" .= mutRecLine
        <*> optionalFieldWithDefault' "end_line" 0 .= mutRecEndLine
        <*> requiredField' "col_start" .= mutRecColStart
        <*> requiredField' "col_end" .= mutRecColEnd
        <*> optionalFieldWith' "source_file" relFileCodec .= mutRecSourceFile
        <*> optionalFieldWithDefault' "source_lines" [] .= mutRecSourceLines
        <*> optionalFieldWithDefault' "mutated_lines" [] .= mutRecMutatedLines
        <*> optionalFieldWithDefault' "context_before" [] .= mutRecContextBefore
        <*> optionalFieldWithDefault' "context_after" [] .= mutRecContextAfter
        <*> optionalFieldWith' "covering_tests" coveringTestsMapCodec .= mutRecCoveringTests

-- | Codec for 'Path Rel File' as a JSON string.
relFileCodec :: JSONCodec (Path Rel File)
relFileCodec =
  bimapCodec
    (\s -> maybe (Left ("invalid relative file path: " ++ T.unpack s)) Right (parseRelFile (T.unpack s)))
    (T.pack . fromRelFile)
    codec

-- | A group of mutation records produced by applying one operator at one
-- source location.  Every group has at least one record in practice, but the
-- type does not enforce non-emptiness — the plugin can drop alternatives
-- during validation.
--
-- Within-group fail-fast (in the runner) skips remaining mutations in a
-- group once one of them survives or is uncovered.
newtype MutationGroup = MutationGroup [MutationRecord]
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec MutationGroup)

instance Validity MutationGroup

instance GenValid MutationGroup where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec MutationGroup where
  codec = dimapCodec MutationGroup (\(MutationGroup rs) -> rs) codec

-- | All mutation groups discovered in one or more modules by the plugin.
newtype MutationManifest = MutationManifest [MutationGroup]
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec MutationManifest)

instance Validity MutationManifest

instance GenValid MutationManifest where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec MutationManifest where
  codec = dimapCodec MutationManifest (\(MutationManifest gs) -> gs) codec

instance Semigroup MutationManifest where
  MutationManifest a <> MutationManifest b = MutationManifest (a <> b)

instance Monoid MutationManifest where
  mempty = MutationManifest []

-- | Write a 'MutationManifest' to @<dir>/<moduleName>.json@.
writeManifestFile :: Path Abs Dir -> String -> MutationManifest -> IO ()
writeManifestFile dir moduleName manifest = do
  ensureDir dir
  fileName <- parseRelFile (moduleName ++ ".json")
  LB.writeFile (fromAbsFile (dir </> fileName)) (Aeson.encode manifest)

-- | Read a 'MutationManifest' from a file, returning 'Nothing' on parse failure.
--
-- Reads strictly so the file handle is closed before this function returns.
readManifestFile :: Path Abs File -> IO (Maybe MutationManifest)
readManifestFile path = Aeson.decodeStrict <$> B.readFile (fromAbsFile path)

-- | Read and concatenate all per-module manifests from a directory.
-- Files that fail to parse are skipped with a warning to stderr.
-- Coverage files (@*.coverage.json@) are excluded; use 'readCoverageDir' for those.
readManifestDir :: Path Abs Dir -> IO MutationManifest
readManifestDir dir = do
  (_, files) <- listDirRel dir
  let jsonFiles = filter isManifestFile files
  mconcat <$> mapM (readOneWith "mutation manifest") jsonFiles
  where
    isManifestFile f =
      fileExtension f == Just ".json"
        && not (isCoverageFile f)
    readOneWith label relFile = do
      result <- readManifestFile (dir </> relFile)
      case result of
        Nothing -> do
          hPutStrLn stderr $ "mutation: failed to decode " ++ label ++ " " ++ fromRelFile relFile
          pure mempty
        Just m -> pure m

-- | Write a coverage manifest to @<dir>/<moduleName>.coverage.json@.
writeCoverageFile :: Path Abs Dir -> String -> MutationManifest -> IO ()
writeCoverageFile dir moduleName manifest = do
  ensureDir dir
  fileName <- parseRelFile (moduleName ++ ".coverage.json")
  LB.writeFile (fromAbsFile (dir </> fileName)) (Aeson.encode manifest)

-- | Read and concatenate all per-module coverage files (@*.coverage.json@) from a directory.
-- Plain manifest files (@*.json@) are excluded.
readCoverageDir :: Path Abs Dir -> IO MutationManifest
readCoverageDir dir = do
  (_, files) <- listDirRel dir
  let coverageFiles = filter isCoverageFile files
  mconcat <$> mapM readOne coverageFiles
  where
    readOne relFile = do
      result <- readManifestFile (dir </> relFile)
      case result of
        Nothing -> do
          hPutStrLn stderr $ "mutation: failed to decode coverage file " ++ fromRelFile relFile
          pure mempty
        Just m -> pure m

isCoverageFile :: Path Rel File -> Bool
isCoverageFile f = case splitExtension f of
  Just (base, ".json") -> case splitExtension base of
    Just (_, ".coverage") -> True
    _ -> False
  _ -> False

-- | A mutation site that was just recorded by the plugin.
newtype MutationAddedEvent = MutationAddedEvent
  { mutationAddedRecord :: MutationRecord
  }
