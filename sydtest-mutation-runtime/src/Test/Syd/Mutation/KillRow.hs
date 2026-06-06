{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.KillRow
  ( TestKillRow (..),
    readTestKillRowFile,
    writeTestKillRowFile,
  )
where

import Autodocodec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.GenValidity.Containers ()
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Test.Syd.Mutation.TestId (TestId)

-- | The per-test outcome of running one mutation's covering tests with that
-- mutation active: 'True' means the test failed (it caught\/killed the
-- mutation), 'False' means it passed (it did not catch the mutation).
--
-- One 'TestKillRow' is the kill-matrix row for a single mutation; the parent
-- driver aggregates the rows of every mutation into the per-test kill sets the
-- redundancy analysis consumes.
newtype TestKillRow = TestKillRow (Map.Map TestId Bool)
  deriving (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec TestKillRow)

instance Validity TestKillRow

instance GenValid TestKillRow where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

-- Encoded as a JSON array of {test_id, killed} objects because TestId is not a
-- valid JSON object key (mirrors 'Test.Syd.Mutation.TestCoverageMap').
instance HasCodec TestKillRow where
  codec =
    dimapCodec
      (TestKillRow . Map.fromList)
      (\(TestKillRow m) -> Map.toList m)
      ( listCodec $
          object "TestKillEntry" $
            (,)
              <$> requiredField' "test_id" .= fst
              <*> requiredField' "killed" .= snd
      )

-- | Write a 'TestKillRow' to the given file path.
writeTestKillRowFile :: FilePath -> TestKillRow -> IO ()
writeTestKillRowFile path m =
  LB.writeFile path (encodeJSONViaCodec m)

-- | Read a 'TestKillRow' from the given file path, returning the aeson error
-- message on parse failure.  Reads strictly so the handle is closed before
-- returning.
readTestKillRowFile :: FilePath -> IO (Either String TestKillRow)
readTestKillRowFile path =
  eitherDecodeJSONViaCodec . LB.fromStrict <$> B.readFile path
