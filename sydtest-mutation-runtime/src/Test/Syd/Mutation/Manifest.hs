{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Manifest
  ( MutationRecord (..),
    MutationManifest (..),
    readManifestFile,
    writeManifestFile,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode, object, withArray, withObject, (.:), (.=))
import qualified Data.ByteString.Lazy as LB
import Path
import Path.IO (ensureDir)
import Test.Syd.Mutation.Runtime (MutationId (..))

-- | One discovered mutation site, as recorded by the plugin.
data MutationRecord = MutationRecord
  { mutRecId :: MutationId,
    mutRecOperator :: String,
    mutRecOriginal :: String,
    mutRecReplacement :: String
  }
  deriving (Show)

instance ToJSON MutationRecord where
  toJSON MutationRecord {mutRecId = MutationId parts, mutRecOperator, mutRecOriginal, mutRecReplacement} =
    object
      [ "id" .= parts,
        "operator" .= mutRecOperator,
        "original" .= mutRecOriginal,
        "replacement" .= mutRecReplacement
      ]

instance FromJSON MutationRecord where
  parseJSON = withObject "MutationRecord" $ \o ->
    MutationRecord . MutationId
      <$> o .: "id"
      <*> o .: "operator"
      <*> o .: "original"
      <*> o .: "replacement"

-- | All mutation sites discovered in a single module by the plugin.
newtype MutationManifest = MutationManifest [MutationRecord]
  deriving (Show)

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
