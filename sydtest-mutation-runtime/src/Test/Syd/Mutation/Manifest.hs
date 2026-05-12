{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Mutation.Manifest
  ( MutationRecord (..),
    MutationManifest (..),
    readManifestFile,
    readManifestDir,
    writeManifestFile,
    readCoverageDir,
    writeCoverageFile,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode, object, withArray, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Path
import Path.IO (ensureDir, listDirRel)
import System.IO (hPutStrLn, stderr)
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId (..), parseTestIdFilterArg, renderTestId)

-- | One discovered mutation site, as recorded by the plugin.
data MutationRecord = MutationRecord
  { mutRecId :: MutationId,
    mutRecOperator :: Text,
    mutRecOriginal :: Text,
    mutRecReplacement :: Text,
    -- | Haskell module name containing the mutation site (e.g. @"Foo.Bar"@).
    mutRecModule :: Text,
    -- | 1-based source line number of the mutated expression.
    mutRecLine :: Word,
    -- | 1-based start column of the mutated expression.
    mutRecColStart :: Word,
    -- | 1-based end column of the mutated expression.
    mutRecColEnd :: Word,
    -- | Source file path relative to the project root, as reported by GHC.
    mutRecSourceFile :: Maybe (Path Rel File),
    -- | Verbatim source line containing the mutated expression.
    mutRecSourceLine :: Maybe Text,
    -- | The source line with the mutation applied (replacement source spliced in).
    mutRecMutatedLine :: Maybe Text,
    -- | Up to 3 source lines immediately before the mutated line.
    mutRecContextBefore :: [Text],
    -- | Up to 3 source lines immediately after the mutated line.
    mutRecContextAfter :: [Text],
    -- | Tests whose execution reaches this mutation site.
    -- 'Nothing' means coverage has not been collected yet.
    mutRecCoveringTests :: Maybe [TestId]
  }
  deriving (Show)

instance ToJSON MutationRecord where
  toJSON MutationRecord {mutRecId = MutationId parts, mutRecOperator, mutRecOriginal, mutRecReplacement, mutRecModule, mutRecLine, mutRecColStart, mutRecColEnd, mutRecSourceFile, mutRecSourceLine, mutRecMutatedLine, mutRecContextBefore, mutRecContextAfter, mutRecCoveringTests} =
    object
      [ "id" .= parts,
        "operator" .= mutRecOperator,
        "original" .= mutRecOriginal,
        "replacement" .= mutRecReplacement,
        "module" .= mutRecModule,
        "line" .= mutRecLine,
        "col_start" .= mutRecColStart,
        "col_end" .= mutRecColEnd,
        "source_file" .= fmap fromRelFile mutRecSourceFile,
        "source_line" .= mutRecSourceLine,
        "mutated_line" .= mutRecMutatedLine,
        "context_before" .= mutRecContextBefore,
        "context_after" .= mutRecContextAfter,
        "covering_tests" .= fmap (map renderTestId) mutRecCoveringTests
      ]

instance FromJSON MutationRecord where
  parseJSON = withObject "MutationRecord" $ \o -> do
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
    mCoveringRaw <- o .:? "covering_tests"
    let coveringTests = fmap (mapMaybe parseTestIdFilterArg) (mCoveringRaw :: Maybe [Text])
    pure
      MutationRecord
        { mutRecId = mid,
          mutRecOperator = op,
          mutRecOriginal = orig,
          mutRecReplacement = repl,
          mutRecModule = modName,
          mutRecLine = line,
          mutRecColStart = colStart,
          mutRecColEnd = colEnd,
          mutRecSourceFile = srcFile,
          mutRecSourceLine = srcLine,
          mutRecMutatedLine = mutatedLine,
          mutRecContextBefore = ctxBefore,
          mutRecContextAfter = ctxAfter,
          mutRecCoveringTests = coveringTests
        }

-- | All mutation sites discovered in one or more modules by the plugin.
newtype MutationManifest = MutationManifest [MutationRecord]
  deriving (Show)

instance Semigroup MutationManifest where
  MutationManifest a <> MutationManifest b = MutationManifest (a <> b)

instance Monoid MutationManifest where
  mempty = MutationManifest []

instance ToJSON MutationManifest where
  toJSON (MutationManifest records) = toJSON records

instance FromJSON MutationManifest where
  parseJSON = withArray "MutationManifest" $ \arr ->
    MutationManifest <$> mapM parseJSON (foldr (:) [] arr)

-- | Write a 'MutationManifest' to @<dir>/<moduleName>.json@.
writeManifestFile :: Path Abs Dir -> String -> MutationManifest -> IO ()
writeManifestFile dir moduleName manifest = do
  ensureDir dir
  fileName <- parseRelFile (moduleName ++ ".json")
  LB.writeFile (fromAbsFile (dir </> fileName)) (encode manifest)

-- | Read a 'MutationManifest' from a file, returning 'Nothing' on parse failure.
readManifestFile :: Path Abs File -> IO (Maybe MutationManifest)
readManifestFile path = decode <$> LB.readFile (fromAbsFile path)

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
  LB.writeFile (fromAbsFile (dir </> fileName)) (encode manifest)

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
