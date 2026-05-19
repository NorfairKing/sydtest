{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The child-process entry point invoked once per leaf test by the
-- coverage-collection phase of the mutation driver.
--
-- Runs a single test with the coverage-collection 'IORef' installed,
-- writes its 'TestCoverageMap' and 'TestBaselineMap' to the configured
-- files, and exits.
module Test.Syd.MutationMode.SingleCoverage
  ( runSingleCoverageMode,
  )
where

import Control.Monad (when)
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Clock (getMonotonicTimeNSec)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import Test.Syd.Def
import Test.Syd.Mutation.Forest (filterTestForestByTrie, testIdTrieFromList)
import Test.Syd.Mutation.Runtime (withCoverageSlot)
import Test.Syd.Mutation.TestBaselineMap (TestBaselineMap (..), writeTestBaselineMapFile)
import Test.Syd.Mutation.TestCoverageMap (TestCoverageMap (..), writeTestCoverageMapFile)
import Test.Syd.Mutation.TestId (parseTestIdFilterArg, renderTestId)
import Test.Syd.MutationMode.Common (diffMonotonicMicros)
import Test.Syd.OptParse
import Test.Syd.Output (printOutputSpecForest)
import Test.Syd.Run (Timed (..))
import Test.Syd.Runner.Synchronous (runSpecForestSynchronously)
import Test.Syd.SpecDef (shouldExitFail)
import Text.Colour (chunk, fore, hPutChunksLocaleWith, red, unlinesChunks)

-- | Child process: run the single test identified by @--mutation-coverage-one@,
-- write its 'TestCoverageMap' to @--mutation-coverage-output@, write its
-- monotonic-clock baseline to @--mutation-coverage-baseline-output@, and exit.
runSingleCoverageMode :: Settings -> Bool -> CoverageChildSettings -> Spec -> IO ()
runSingleCoverageMode settings failFast covChild spec = do
  tid <- case parseTestIdFilterArg (coverageChildTestId covChild) of
    Nothing -> fail "runSingleCoverageMode: no valid coverage-child test id"
    Just t -> pure t
  let outputFile = coverageChildOutput covChild
      baselineFile = coverageChildBaselineOutput covChild
  specForest <- execTestDefM settings spec
  let coverageSettings =
        settings
          { settingThreads = Synchronous,
            settingMaxSuccess = 1
          }
      trie = testIdTrieFromList [tid]
      filtered = filterTestForestByTrie trie specForest
  ref <- newIORef Set.empty
  startTime <- getMonotonicTimeNSec
  resultForest <- withCoverageSlot ref $ runSpecForestSynchronously coverageSettings filtered
  endTime <- getMonotonicTimeNSec
  covered <- readIORef ref
  let coverageMap = TestCoverageMap (Map.singleton tid covered)
      elapsedMicros = diffMonotonicMicros endTime startTime
  writeTestCoverageMapFile outputFile coverageMap
  writeTestBaselineMapFile baselineFile (TestBaselineMap (Map.singleton tid elapsedMicros))
  -- Mutation testing only makes sense against a passing baseline: if a test
  -- is red before any mutation is applied, its mutation scores are
  -- meaningless.  Print the offending test's output and a loud warning in
  -- both fail-fast and non-fail-fast cases.  Under fail-fast, also exit with
  -- code 2 so the parent aborts the run (see the coverage-parent runner).
  when (shouldExitFail settings (timedValue resultForest)) $ do
    printOutputSpecForest settings resultForest
    hPutChunksLocaleWith (settingTerminalCapabilities settings) stderr $
      unlinesChunks
        [ [ fore red $ chunk "coverage: WARNING: test failed during baseline run for ",
            fore red $ chunk (renderTestId tid),
            fore red $ chunk " — mutation scores against this baseline are unreliable"
          ]
        ]
    when failFast $ exitWith (ExitFailure 2)
