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
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import Test.Syd.Mutation.TestId (TestId)

-- | Per-test wall-clock baselines (microseconds) collected during the
-- coverage phase.  Used to derive per-mutation timeouts in the mutation
-- phase.
newtype TestBaselineMap = TestBaselineMap (Map.Map TestId Word)
  deriving stock (Show)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec TestBaselineMap)

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
  LB.writeFile path (Aeson.encode m)

readTestBaselineMapFile :: FilePath -> IO (Maybe TestBaselineMap)
readTestBaselineMapFile path =
  Aeson.decode <$> LB.readFile path
