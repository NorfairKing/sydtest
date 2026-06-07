{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.KillRow
  ( TestKillRow (..),
    buildKillRow,
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

-- | Pair the canonical covering 'TestId's (in source order) with the per-test
-- killed flags (in result-forest order) into a 'TestKillRow'.
--
-- The two lists are expected to line up positionally; a length mismatch means
-- that invariant was violated (the id-assignment and result walks disagreed),
-- so we return 'Left' rather than silently producing a misaligned row.  Callers
-- treat 'Left' as "no row for this mutation" — they must NOT let it change the
-- mutation's killed\/survived verdict.
buildKillRow :: [TestId] -> [Bool] -> Either String TestKillRow
buildKillRow tids flags
  | length tids /= length flags =
      Left
        ( "buildKillRow: covering tests ("
            ++ show (length tids)
            ++ ") and result leaves ("
            ++ show (length flags)
            ++ ") disagree"
        )
  | otherwise = Right (TestKillRow (Map.fromList (zip tids flags)))

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
