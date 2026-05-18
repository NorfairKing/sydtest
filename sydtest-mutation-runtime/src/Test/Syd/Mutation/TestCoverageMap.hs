{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.TestCoverageMap
  ( TestCoverageMap (..),
    readTestCoverageMapFile,
    writeTestCoverageMapFile,
  )
where

import Autodocodec
import Data.Bifunctor (second)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Syd.Mutation.Runtime (MutationId)
import Test.Syd.Mutation.TestId (TestId)

-- | Coverage result for one or more tests: maps each 'TestId' to the set of
-- 'MutationId's reached during that test's execution.
newtype TestCoverageMap = TestCoverageMap (Map.Map TestId (Set MutationId))
  deriving (Show)

-- Encoded as a JSON array of {test_id, mutations} objects because TestId is
-- not a valid JSON object key.
instance HasCodec TestCoverageMap where
  codec =
    dimapCodec
      (TestCoverageMap . Map.fromList . map (second Set.fromList))
      (\(TestCoverageMap m) -> map (second Set.toList) (Map.toList m))
      ( listCodec $
          object "TestCoverageEntry" $
            (,)
              <$> requiredField' "test_id" .= fst
              <*> requiredField' "mutations" .= snd
      )

instance Semigroup TestCoverageMap where
  TestCoverageMap a <> TestCoverageMap b = TestCoverageMap (Map.unionWith (<>) a b)

instance Monoid TestCoverageMap where
  mempty = TestCoverageMap Map.empty

-- | Write a 'TestCoverageMap' to the given file path.
writeTestCoverageMapFile :: FilePath -> TestCoverageMap -> IO ()
writeTestCoverageMapFile path m =
  LB.writeFile path (encodeJSONViaCodec m)

-- | Read a 'TestCoverageMap' from the given file path.
-- Returns 'Nothing' on parse failure.
--
-- Reads strictly (via 'B.readFile' + 'eitherDecodeJSONViaCodec') so the file
-- handle is closed before this function returns.  Defensive against a
-- suspected (but unproven) contributor to 'BlockedIndefinitelyOnMVar'
-- loops at the coverage/mutation phase boundary on large projects.
readTestCoverageMapFile :: FilePath -> IO (Maybe TestCoverageMap)
readTestCoverageMapFile path =
  either (const Nothing) Just . eitherDecodeJSONViaCodec . LB.fromStrict
    <$> B.readFile path
