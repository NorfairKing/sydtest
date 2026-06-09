{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.TestBaselineMap
  ( TestBaselineMap (..),
    readTestBaselineMapFile,
    writeTestBaselineMapFile,
  )
where

import Autodocodec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.GenValidity.Containers ()
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Test.Syd.Mutation.TestId (TestId)

-- | Per-test monotonic-clock baselines (microseconds) collected during the
-- coverage phase.  Used to derive per-mutation timeouts in the mutation
-- phase.
newtype TestBaselineMap = TestBaselineMap (Map.Map TestId Word)
  deriving (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec TestBaselineMap)

instance Validity TestBaselineMap

instance GenValid TestBaselineMap where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

-- Encoded as a JSON array of {test_id, elapsed_micros} objects because
-- TestId is not a valid JSON object key.
instance HasCodec TestBaselineMap where
  codec =
    dimapCodec
      (TestBaselineMap . Map.fromList)
      (\(TestBaselineMap m) -> Map.toList m)
      ( listCodec $
          object "TestBaselineEntry" $
            (,)
              <$> requiredField' "test_id" .= fst
              <*> requiredField' "elapsed_micros" .= snd
      )

-- | Merge by taking the maximum elapsed time seen for each test. Coverage
-- children are run with @settingThreads = Synchronous@ but a test may be
-- exercised across multiple suites, and the safer baseline is the slowest.
instance Semigroup TestBaselineMap where
  TestBaselineMap a <> TestBaselineMap b = TestBaselineMap (Map.unionWith max a b)

instance Monoid TestBaselineMap where
  mempty = TestBaselineMap Map.empty

writeTestBaselineMapFile :: FilePath -> TestBaselineMap -> IO ()
writeTestBaselineMapFile path m =
  LB.writeFile path (encodeJSONViaCodec m)

-- | Read the baseline map strictly (via 'SB.readFile' +
-- 'eitherDecodeJSONViaCodec') so the file handle is closed before this
-- function returns.  Defensive against a suspected (but unproven) contributor
-- to 'BlockedIndefinitelyOnMVar' loops at the coverage/mutation phase
-- boundary on large projects.  Returns the aeson error message on parse
-- failure.
readTestBaselineMapFile :: FilePath -> IO (Either String TestBaselineMap)
readTestBaselineMapFile path =
  eitherDecodeJSONViaCodec . LB.fromStrict <$> SB.readFile path
