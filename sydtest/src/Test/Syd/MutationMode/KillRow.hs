{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The child-process entry point invoked once per mutation by the redundancy
-- kill-matrix runner.
--
-- Filters the spec to the tests covering the requested mutation, sets the
-- mutation active, runs them synchronously with /within-set fail-fast off/ so
-- every covering test runs, and records each covering test's pass\/fail as a
-- 'TestKillRow' (the kill-matrix row for that mutation).
--
-- Covering-test 'TestId's are assigned by flattening the /full/ definition
-- forest (the canonical assignment, matching the coverage phase) and then
-- positionally zipping with the result forest's leaves: filtering and the
-- synchronous run both preserve source order, and randomisation is disabled, so
-- the two line up.
module Test.Syd.MutationMode.KillRow
  ( runKillRowMode,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Test.Syd.Def
import Test.Syd.Mutation.AugmentedManifest
  ( augmentedMutationRecordCoveringTests,
    lookupAugmentedMutationRecord,
    readAugmentedManifestFile,
  )
import Test.Syd.Mutation.Forest (filterTestForestByTrie, flattenTestForestWithIds, testIdTrieFromList)
import Test.Syd.Mutation.KillRow (TestKillRow (..), writeTestKillRowFile)
import Test.Syd.Mutation.Runtime (parseMutationId, renderMutationId, setActiveMutation)
import Test.Syd.Mutation.TestId (TestId)
import Test.Syd.OptParse
import Test.Syd.Run (Timed (..))
import Test.Syd.Runner.Synchronous (runSpecForestSynchronously)
import Test.Syd.SpecDef
import Test.Syd.SpecForest

-- | Child process: run one mutation's covering tests with the mutation active,
-- record each test's pass\/fail, write the 'TestKillRow', and exit 0.
runKillRowMode :: Settings -> KillRowChildSettings -> Spec -> IO ()
runKillRowMode settings killChild spec = do
  mid <- case parseMutationId (killRowChildId killChild) of
    Nothing -> fail "runKillRowMode: no valid kill-row-child mutation id"
    Just m -> pure m
  augmented <- readAugmentedManifestFile (killRowChildAugmentedManifestDir killChild)
  specForest <- execTestDefM settings spec
  let coveringTestsMap =
        maybe
          Map.empty
          augmentedMutationRecordCoveringTests
          (lookupAugmentedMutationRecord mid augmented)
      coveringTests :: [TestId]
      coveringTests = case killRowChildSuiteName killChild of
        Just suiteName -> fromMaybe [] (Map.lookup suiteName coveringTestsMap)
        Nothing -> concatMap snd (Map.toList coveringTestsMap)
      coveringSet = Set.fromList coveringTests
      -- Canonical ids from the full forest, kept in source order and restricted
      -- to the covering set.  These line up positionally with the result leaves.
      orderedCovering =
        [ tid
        | (tid, ()) <- flattenTestForestWithIds specForest,
          tid `Set.member` coveringSet
        ]
      filtered = filterTestForestByTrie (testIdTrieFromList coveringTests) specForest
      -- Synchronous, fail-fast OFF (so every covering test runs and is
      -- recorded), randomisation OFF (so result order matches the canonical
      -- flatten order).
      runSettings =
        settings
          { settingThreads = Synchronous,
            settingFailFast = False,
            settingRandomiseExecutionOrder = False
          }
  setActiveMutation (Just mid)
  resultForest <- runSpecForestSynchronously runSettings filtered
  setActiveMutation Nothing
  let leaves = collectResultLeaves (timedValue resultForest)
      killedFlags = map (testRunReportFailed settings . timedValue) leaves
  if length orderedCovering /= length leaves
    then
      fail $
        "runKillRowMode: covering tests ("
          ++ show (length orderedCovering)
          ++ ") and result leaves ("
          ++ show (length leaves)
          ++ ") disagree for mutation "
          ++ renderMutationId mid
    else
      writeTestKillRowFile
        (killRowChildOutput killChild)
        (TestKillRow (Map.fromList (zip orderedCovering killedFlags)))

-- | Collect each leaf's timed 'TestRunReport' from a 'ResultForest', in source
-- order (left-to-right, depth-first).  Mirrors the structure the canonical
-- 'TestId' flatten walks, so the two stay positionally aligned.
collectResultLeaves :: ResultForest -> [Timed TestRunReport]
collectResultLeaves = concatMap go
  where
    go = \case
      SpecifyNode _ tdef -> [testDefVal tdef]
      PendingNode _ _ -> []
      DescribeNode _ sub -> collectResultLeaves sub
      SubForestNode sub -> collectResultLeaves sub
