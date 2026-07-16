{-# LANGUAGE OverloadedStrings #-}

-- | The child-process entry point invoked once per mutation when the parent
-- mutation runner spawns it.  Filters the spec to the tests that cover the
-- requested mutation, sets the mutation as active, and runs them synchronously
-- under fail-fast.
module Test.Syd.MutationMode.Single
  ( runSingleMutationMode,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import Test.Syd.Def
import Test.Syd.Mutation.AugmentedManifest
  ( augmentedMutationRecordCoveringTests,
    lookupAugmentedMutationRecord,
    readAugmentedManifestFile,
  )
import Test.Syd.Mutation.Forest (filterTestForestByTrie, reorderForMutationChild, testIdTrieFromList)
import Test.Syd.Mutation.Runtime (parseMutationId, setActiveMutation)
import Test.Syd.Mutation.TestBaselineMap (TestBaselineMap (..), readTestBaselineMapDirIfExists)
import Test.Syd.Mutation.TestId (TestId)
import Test.Syd.OptParse
import Test.Syd.Output (printOutputSpecForest)
import Test.Syd.Run (Timed (..))
import Test.Syd.Runner.Synchronous (runSpecForestSynchronously)
import Test.Syd.SpecDef (shouldExitFail)

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
  -- Order the covering tests cheapest-first so the fail-fast run reaches a
  -- killing test sooner.  This is a drop-in alternative to execution-order
  -- randomisation, so 'reorderForMutationChild' only reorders when the suite
  -- has randomisation enabled; if the author fixed the order
  -- (--no-randomise-execution-order), the forest is left as-is.  'execTestDefM'
  -- above already ran the (seeded) shuffle that keeps ids in sync with the
  -- coverage phase, which is why the reorder happens after filtering.
  mBaseline <- readTestBaselineMapDirIfExists (mutationChildAugmentedManifestDir mutChild)
  let orderedForest =
        reorderForMutationChild
          (settingRandomiseExecutionOrder settings)
          (fmap (\(TestBaselineMap costs) -> costs) mBaseline)
          forest
  setActiveMutation (Just mid)
  timedResult <- runSpecForestSynchronously (settings {settingThreads = Synchronous, settingFailFast = True}) orderedForest
  setActiveMutation Nothing
  printOutputSpecForest settings timedResult
  if shouldExitFail settings (timedValue timedResult)
    then exitWith (ExitFailure 1)
    else exitSuccess
