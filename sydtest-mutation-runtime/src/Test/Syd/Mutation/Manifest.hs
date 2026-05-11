{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Manifest
  ( MutationRecord (..),
    MutationManifest (..),
    readManifestFile,
    readManifestDir,
    writeManifestFile,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode, object, withArray, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import Path
import Path.IO (ensureDir, listDirRel)
import System.IO (hPutStrLn, stderr)
import Test.Syd.Mutation.Runtime (MutationId (..))

-- | One discovered mutation site, as recorded by the plugin.
data MutationRecord = MutationRecord
  { mutRecId :: MutationId,
    mutRecOperator :: String,
    mutRecOriginal :: String,
    mutRecReplacement :: String,
    -- | Verbatim source line containing the mutated expression.
    mutRecSourceLine :: Maybe Text,
    -- | Up to 3 source lines immediately before the mutated line.
    mutRecContextBefore :: [Text],
    -- | Up to 3 source lines immediately after the mutated line.
    mutRecContextAfter :: [Text]
  }
  deriving (Show)

instance ToJSON MutationRecord where
  toJSON MutationRecord {mutRecId = MutationId parts, mutRecOperator, mutRecOriginal, mutRecReplacement, mutRecSourceLine, mutRecContextBefore, mutRecContextAfter} =
    object
      [ "id" .= parts,
        "operator" .= mutRecOperator,
        "original" .= mutRecOriginal,
        "replacement" .= mutRecReplacement,
        "source_line" .= mutRecSourceLine,
        "context_before" .= mutRecContextBefore,
        "context_after" .= mutRecContextAfter
      ]

instance FromJSON MutationRecord where
  parseJSON = withObject "MutationRecord" $ \o -> do
    mid <- MutationId <$> o .: "id"
    op <- o .: "operator"
    orig <- o .: "original"
    repl <- o .: "replacement"
    srcLine <- o .:? "source_line"
    ctxBefore <- o .:? "context_before" .!= []
    ctxAfter <- o .:? "context_after" .!= []
    pure
      MutationRecord
        { mutRecId = mid,
          mutRecOperator = op,
          mutRecOriginal = orig,
          mutRecReplacement = repl,
          mutRecSourceLine = srcLine,
          mutRecContextBefore = ctxBefore,
          mutRecContextAfter = ctxAfter
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
readManifestDir :: Path Abs Dir -> IO MutationManifest
readManifestDir dir = do
  (_, files) <- listDirRel dir
  let jsonFiles = filter (\f -> fileExtension f == Just ".json") files
  mconcat <$> mapM readOne jsonFiles
  where
    readOne relFile = do
      result <- readManifestFile (dir </> relFile)
      case result of
        Nothing -> do
          hPutStrLn stderr $ "mutation: failed to decode " ++ fromRelFile relFile
          pure mempty
        Just m -> pure m
