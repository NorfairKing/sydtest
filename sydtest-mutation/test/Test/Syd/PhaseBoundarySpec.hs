{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Smoke tests around bug #1 (the @<<loop>>@ that surfaces in
-- 'runCoverageMode' on large projects).
--
-- These tests exercise the read-then-tear-down and read-merge-write
-- patterns that the coverage harness uses internally.  They do NOT
-- reliably reproduce the original @<<loop>>@ — that requires the full
-- nix-ci scenario (~1500 tests, tmp-postgres, 8 concurrent jobs).  They
-- serve as a regression guard against gross mistakes in those code
-- paths: if the read/merge/write round-trip stops preserving data, or
-- if 'mapConcurrently' over many per-child temp dirs starts looping,
-- these tests will catch it.
--
-- Determinism note: 'mapConcurrently' is non-deterministic in
-- scheduling, but the assertions only check structural invariants
-- (counts, no failure to decode), so flakiness is not expected.
module Test.Syd.PhaseBoundarySpec (spec) where

import Control.Concurrent.Async (mapConcurrently)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Path
import Path.IO (withSystemTempDir)
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestBaselineMap
  ( TestBaselineMap (..),
    readTestBaselineMapFile,
    writeTestBaselineMapFile,
  )
import Test.Syd.Mutation.TestCoverageMap
  ( TestCoverageMap (..),
    readTestCoverageMapFile,
    writeTestCoverageMapFile,
  )
import Test.Syd.Mutation.TestId (TestId (..))

aTestId :: Int -> TestId
aTestId i = TestId ((T.pack ('t' : show i), 0) :| [])

spec :: Spec
spec = describe "phase boundary smoke (bug #1)" $ do
  it "concurrent per-child read-then-teardown of coverage maps preserves data" $ do
    -- Mirrors the runCoverageChild pattern: per-thread temp dir, write
    -- result, read it back, then let the temp dir get torn down.
    let n = 500
        readOne i = withSystemTempDir "phase-boundary-cov" $ \dir -> do
          let path = fromAbsFile (dir </> [relfile|coverage.json|])
              cm = TestCoverageMap (Map.singleton (aTestId i) (Set.singleton (MutationId ["M", "Op", show i, "1", "5", "r", "1"])))
          writeTestCoverageMapFile path cm
          eRead <- readTestCoverageMapFile path
          case eRead of
            Left err -> expectationFailure ("decode failed: " ++ err)
            Right (TestCoverageMap m) -> Map.size m `shouldBe` 1
    _ <- mapConcurrently readOne [1 .. n]
    pure ()

  it "concurrent per-child read-then-teardown of baseline maps preserves data" $ do
    let n = 500
        readOne i = withSystemTempDir "phase-boundary-base" $ \dir -> do
          let path = fromAbsFile (dir </> [relfile|baseline.json|])
              bm = TestBaselineMap (Map.singleton (aTestId i) (fromIntegral i))
          writeTestBaselineMapFile path bm
          eRead <- readTestBaselineMapFile path
          case eRead of
            Left err -> expectationFailure ("decode failed: " ++ err)
            Right (TestBaselineMap m) -> Map.size m `shouldBe` 1
    _ <- mapConcurrently readOne [1 .. n]
    pure ()

  it "many sequential read-merge-write rounds against the same file preserve data" $
    -- Mirrors the multi-suite augmented-manifest flow where one file is
    -- repeatedly read, merged, and written back in the same process.
    withSystemTempDir "phase-boundary-multi" $ \dir -> do
      let mkRec :: Int -> Int -> AugmentedMutationRecord
          mkRec i suite =
            AugmentedMutationRecord
              { augmentedMutationRecordId = MutationId ["M", "Op", show i, "1", "5", "r", "1"],
                augmentedMutationRecordOperator = "Op",
                augmentedMutationRecordOriginal = "+",
                augmentedMutationRecordReplacement = "-",
                augmentedMutationRecordModule = "M",
                augmentedMutationRecordLine = fromIntegral i,
                augmentedMutationRecordEndLine = fromIntegral i,
                augmentedMutationRecordColStart = 1,
                augmentedMutationRecordColEnd = 5,
                augmentedMutationRecordSourceFile = Just $(mkRelFile "src/M.hs"),
                augmentedMutationRecordSourceLines = [],
                augmentedMutationRecordMutatedLines = [],
                augmentedMutationRecordContextBefore = [],
                augmentedMutationRecordContextAfter = [],
                augmentedMutationRecordCoveringTests =
                  Map.singleton (T.pack ("suite-" ++ show suite)) [],
                augmentedMutationRecordTimeoutMicros = 30000000
              }
          newSuite :: Int -> Int -> AugmentedManifest
          newSuite suite recs =
            -- One singleton group per record keeps the merge semantics
            -- (combine by id) easy to reason about.
            AugmentedManifest [AugmentedMutationGroup [mkRec i suite] | i <- [1 .. recs]]
          recsPerSuite = 200
          totalSuites = 50
      writeAugmentedManifestFile dir (newSuite 0 recsPerSuite)
      let loop s
            | s > totalSuites = pure ()
            | otherwise = do
                mPrev <- readAugmentedManifestFileIfExists dir
                case mPrev of
                  Nothing -> expectationFailure "expected manifest"
                  Just prev -> do
                    let merged = mergeAugmentedManifests prev (newSuite s recsPerSuite)
                    writeAugmentedManifestFile dir merged
                    loop (s + 1)
      loop 1
      AugmentedManifest finalGroups <- readAugmentedManifestFile dir
      let finalRecords = [r | AugmentedMutationGroup rs <- finalGroups, r <- rs]
      length finalRecords `shouldBe` recsPerSuite
      -- All suites should appear in the merged covering_tests maps.
      let allSuites = Map.unionsWith (++) (map augmentedMutationRecordCoveringTests finalRecords)
      length (Map.keys allSuites) `shouldBe` totalSuites + 1
