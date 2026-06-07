{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The child-process entry point invoked once per mutation when the parent
-- mutation runner spawns it.  Filters the spec to the tests that cover the
-- requested mutation, sets the mutation as active, and runs them synchronously.
--
-- When @--mutation-kill-row-output@ is given, the child additionally records
-- each covering test's pass\/fail (the kill-matrix row for this mutation, used
-- by the redundant-test analysis) and runs with within-set fail-fast OFF so
-- that every covering test runs and is recorded.  Otherwise it runs under
-- fail-fast (stopping at the first test that kills the mutation), which is
-- cheaper.
module Test.Syd.MutationMode.Single
  ( runSingleMutationMode,
  )
where

import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as Set
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import Test.Syd.Def
import Test.Syd.Mutation.AugmentedManifest
  ( augmentedMutationRecordCoveringTests,
    lookupAugmentedMutationRecord,
    readAugmentedManifestFile,
  )
import Test.Syd.Mutation.Forest (filterTestForestByTrie, flattenTestForestWithIds, testIdTrieFromList)
import Test.Syd.Mutation.KillRow (TestKillRow (..), writeTestKillRowFile)
import Test.Syd.Mutation.Runtime (parseMutationId, setActiveMutation)
import Test.Syd.Mutation.TestId (TestId)
import Test.Syd.OptParse
import Test.Syd.Output (printOutputSpecForest)
import Test.Syd.Run (Timed (..))
import Test.Syd.Runner.Synchronous (runSpecForestSynchronously)
import Test.Syd.SpecDef (ResultForest, TDef (..), TestRunReport, shouldExitFail, testRunReportFailed)
import Test.Syd.SpecForest

-- | Child process: run only the tests covering a single mutation and exit
-- with the appropriate exit code.
--
-- When @mutationChildSuiteName@ is set, only the covering tests for that
-- suite are run.  Otherwise the union of all suites' covering tests is used
-- (single-suite / backward-compatible behaviour).
runSingleMutationMode :: Settings -> MutationChildSettings -> Spec -> IO ()
runSingleMutationMode settings mutChild spec = do
  mid <- case parseMutationId (mutationChildId mutChild) of
    Nothing -> fail "runSingleMutationMode: no valid mutation-child id"
    Just m -> pure m
  augmented <- readAugmentedManifestFile (mutationChildAugmentedManifestDir mutChild)
  specForest <- execTestDefM settings spec
  let coveringTestsMap =
        maybe
          Map.empty
          augmentedMutationRecordCoveringTests
          (lookupAugmentedMutationRecord mid augmented)
      coveringTests :: [TestId]
      coveringTests = case mutationChildSuiteName mutChild of
        Just suiteName ->
          fromMaybe [] (Map.lookup suiteName coveringTestsMap)
        Nothing ->
          -- single-suite / backward-compat: union of all suites
          concatMap snd (Map.toList coveringTestsMap)
      forest = case coveringTests of
        [] -> specForest
        ts -> filterTestForestByTrie (testIdTrieFromList ts) specForest
      mKillRowOutput = mutationChildKillRowOutput mutChild
      -- With a kill-row requested we must run every covering test (so each
      -- outcome is recorded), so within-set fail-fast is off; otherwise keep
      -- it on for speed.  Randomisation is off either way so the result forest
      -- lines up with the canonical 'TestId' flatten when building the row.
      failFast = isNothing mKillRowOutput
      -- Canonical covering ids from the full forest, in source order; these
      -- line up positionally with the (filtered) result leaves.
      coveringSet = Set.fromList coveringTests
      orderedCovering =
        [ tid
        | (tid, ()) <- flattenTestForestWithIds specForest,
          tid `Set.member` coveringSet
        ]
  setActiveMutation (Just mid)
  timedResult <-
    runSpecForestSynchronously
      ( settings
          { settingThreads = Synchronous,
            settingFailFast = failFast,
            settingRandomiseExecutionOrder = False
          }
      )
      forest
  setActiveMutation Nothing
  printOutputSpecForest settings timedResult
  for_ mKillRowOutput (writeKillRow settings orderedCovering (timedValue timedResult))
  if shouldExitFail settings (timedValue timedResult)
    then exitWith (ExitFailure 1)
    else exitSuccess

-- | Record the per-test pass\/fail of this mutation's covering tests to a
-- 'TestKillRow' file.
--
-- @orderedCovering@ is the canonical covering 'TestId's in source order
-- (assigned from the full definition forest, matching the coverage phase);
-- @resultForest@ is the filtered run's result.  They line up positionally
-- because filtering and the synchronous run both preserve source order and
-- randomisation is disabled.  A length mismatch means that invariant was
-- violated, so we fail loudly rather than write a misaligned row.
writeKillRow :: Settings -> [TestId] -> ResultForest -> FilePath -> IO ()
writeKillRow settings orderedCovering resultForest outFile
  | null orderedCovering = writeTestKillRowFile outFile (TestKillRow Map.empty)
  | length orderedCovering /= length leaves =
      fail $
        "runSingleMutationMode: covering tests ("
          ++ show (length orderedCovering)
          ++ ") and result leaves ("
          ++ show (length leaves)
          ++ ") disagree"
  | otherwise =
      writeTestKillRowFile outFile (TestKillRow (Map.fromList (zip orderedCovering killedFlags)))
  where
    leaves = collectResultLeaves resultForest
    killedFlags = map (testRunReportFailed settings . timedValue) leaves

-- | Collect each leaf's timed 'TestRunReport' from a 'ResultForest', in source
-- order (left-to-right, depth-first).
collectResultLeaves :: ResultForest -> [Timed TestRunReport]
collectResultLeaves = concatMap go
  where
    go = \case
      SpecifyNode _ tdef -> [testDefVal tdef]
      PendingNode _ _ -> []
      DescribeNode _ sub -> collectResultLeaves sub
      SubForestNode sub -> collectResultLeaves sub
