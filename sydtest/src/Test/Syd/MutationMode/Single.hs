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
import Path
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import Test.Syd.Def
import Test.Syd.Mutation.AugmentedManifest
  ( augmentedMutationRecordCoveringTests,
    lookupAugmentedMutationRecord,
    readAugmentedManifestFile,
  )
import Test.Syd.Mutation.Forest (filterTestForestByTrie, testIdTrieFromList)
import Test.Syd.Mutation.Runtime (parseMutationId, setActiveMutation)
import Test.Syd.Mutation.TestId (TestId)
import Test.Syd.MutationMode (resolveAugmentedManifestDir)
import Test.Syd.OptParse
import Test.Syd.Output (printOutputSpecForest)
import Test.Syd.Run (Timed (..))
import Test.Syd.Runner.Synchronous (runSpecForestSynchronously)
import Test.Syd.SpecDef (shouldExitFail)

-- | Child process: run only the tests covering a single mutation and exit
-- with the appropriate exit code.
--
-- When @settingMutationSuiteName@ is set, only the covering tests for that
-- suite are run.  Otherwise the union of all suites' covering tests is used
-- (single-suite / backward-compatible behaviour).
runSingleMutationMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runSingleMutationMode settings _manifestDirs spec = do
  mid <- case settingMutationOne settings >>= parseMutationId of
    Nothing -> fail "runSingleMutationMode: no valid --mutation-one id"
    Just m -> pure m
  augDir <- resolveAugmentedManifestDir settings
  augmented <- readAugmentedManifestFile augDir
  specForest <- execTestDefM settings spec
  let coveringTestsMap =
        maybe
          Map.empty
          augmentedMutationRecordCoveringTests
          (lookupAugmentedMutationRecord mid augmented)
      coveringTests :: [TestId]
      coveringTests = case settingMutationSuiteName settings of
        Just suiteName ->
          fromMaybe [] (Map.lookup suiteName coveringTestsMap)
        Nothing ->
          -- single-suite / backward-compat: union of all suites
          concatMap snd (Map.toList coveringTestsMap)
      forest = case coveringTests of
        [] -> specForest
        ts -> filterTestForestByTrie (testIdTrieFromList ts) specForest
  setActiveMutation (Just mid)
  timedResult <- runSpecForestSynchronously (settings {settingThreads = Synchronous, settingFailFast = True}) forest
  setActiveMutation Nothing
  printOutputSpecForest settings timedResult
  if shouldExitFail settings (timedValue timedResult)
    then exitWith (ExitFailure 1)
    else exitSuccess
