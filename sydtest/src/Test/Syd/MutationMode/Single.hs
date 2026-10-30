{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The child-process entry point invoked once per mutation when the parent
-- mutation runner spawns it.  Filters the spec to the tests that cover the
-- requested mutation, sets the mutation as active, and runs them synchronously
-- under fail-fast.
module Test.Syd.MutationMode.Single
  ( runSingleMutationMode,
    mutationChildSpecForest,
    mutationChildSettings,
  )
where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Conc (setNumCapabilities)
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
import Test.Syd.SpecDef (TestForest, shouldExitFail)

-- | Child process: run only the tests covering a single mutation and exit
-- with the appropriate exit code.
--
-- When @mutationChildSuiteName@ is set, only the covering tests for that
-- suite are run.  Otherwise the union of all suites' covering tests is used
-- (single-suite / backward-compatible behaviour).
runSingleMutationMode :: Settings -> MutationChildSettings -> Spec -> IO ()
runSingleMutationMode settings mutChild spec = do
  -- The tests run one at a time, and the parent runs one child per
  -- capability.
  -- More capabilities would only make every child run the parallel garbage
  -- collector on every core, and slow the others down.
  -- This is a no-op in a program built without -threaded.
  setNumCapabilities 1
  mid <- case parseMutationId (mutationChildId mutChild) of
    Nothing -> fail "runSingleMutationMode: no valid mutation-child id"
    Just m -> pure m
  augmented <- readAugmentedManifestFile (mutationChildAugmentedManifestDir mutChild)
  specForest <- mutationChildSpecForest settings spec
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
  timedResult <- runSpecForestSynchronously (mutationChildSettings settings) orderedForest
  setActiveMutation Nothing
  printOutputSpecForest settings timedResult
  if shouldExitFail settings (timedValue timedResult)
    then exitWith (ExitFailure 1)
    else exitSuccess

-- | The forest a child runs.
--
-- A test takes its settings, such as how far to shrink, when it is defined, so
-- the forest is defined with the child's settings rather than only run with
-- them.
mutationChildSpecForest :: Settings -> Spec -> IO (TestForest '[] ())
mutationChildSpecForest settings = execTestDefM (mutationChildSettings settings)

-- | A child only reports whether any covering test failed, so it stops at the
-- first failure, does not shrink it, and does not run it again.
-- Shrinking a large generated value can take minutes, once per killed
-- mutation.
-- Running a failure again cannot make it count as a pass, except for a test
-- marked flaky, which sets its own retries.
mutationChildSettings :: Settings -> Settings
mutationChildSettings settings =
  settings
    { settingThreads = Synchronous,
      settingFailFast = True,
      settingMaxShrinks = 0,
      settingRetries = 0
    }
