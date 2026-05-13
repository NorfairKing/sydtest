{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | This module provides four entry points for mutation testing:
--
-- * 'runCoverageMode': parent process — enumerate all tests, spawn one child
--   subprocess per test concurrently to collect coverage, merge results, and
--   write @manifest-augmented.json@.
-- * 'runSingleCoverageMode': child process — run one test, write its
--   'TestCoverageMap' to a file, and exit.
-- * 'runMutationMode': parent process — read the augmented manifest and spawn
--   one child process per mutation.
-- * 'runSingleMutationMode': child process — run only the tests that cover a
--   single mutation and exit with success/failure.
--
-- Running each mutation (and each coverage collection) in a subprocess is
-- necessary for three reasons:
--
-- 1. __Memory limit__: a mutation can cause unbounded allocation (e.g. turning
--    a termination condition into a no-op).  The RTS @-M@ flag terminates the
--    process when the heap cap is hit — this is not a catchable exception.  In
--    a subprocess only the child dies; the parent records it as killed and
--    moves on.
--
-- 2. __Laziness \/ memoisation__: the spec forest is built once and reused
--    across mutations.  Values computed via 'runIO' during spec construction
--    are memoised in the parent\'s heap and will not reflect the active
--    mutation.  Subprocesses get a fresh heap each time, so there is no risk
--    of a memoised value masking a mutation.
--
-- 3. __Parallelism__: because 'activeMutation' and 'coverageSlot' are
--    process-global 'IORef's, both mutations and coverage collection must run
--    serially within a single process.  Subprocesses are independent, so the
--    parent runs up to N children concurrently where N = 'getNumCapabilities'.
module Test.Syd.MutationMode
  ( runMutationMode,
    runSingleMutationMode,
    runCoverageMode,
    runSingleCoverageMode,
  )
where

import Control.Concurrent (newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket_)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Conc (getNumCapabilities)
import Path
import Path.IO (copyFile, getCurrentDir, withSystemTempDir)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (BufferMode (..), IOMode (..), hClose, hPutStr, hSetBuffering, openFile, stderr)
import System.Process.Typed (proc, runProcess, setStderr, setStdout, useHandleOpen)
import Test.Syd.Def
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationRecord (..),
    MutationProgressEvent (..),
    MutationRunReport (..),
    SurvivedMutation (..),
    UncoveredMutation (..),
    fromMutationRecord,
    lookupAugmentedMutationRecord,
    mergeAugmentedManifests,
    readAugmentedManifestFile,
    readAugmentedManifestFileIfExists,
    renderMutationProgressEvent,
    renderMutationRunReport,
    writeAugmentedManifestFile,
    writeMutationRunReport,
  )
import Test.Syd.Mutation.Forest (filterTestForestByTrie, flattenTestForestWithIds, testIdTrieFromList)
import Test.Syd.Mutation.Manifest
  ( MutationManifest (..),
    MutationRecord (..),
    readManifestDir,
  )
import Test.Syd.Mutation.Runtime (MutationId (..), parseMutationId, renderMutationId, setActiveMutation, withCoverageSlot)
import Test.Syd.Mutation.TestCoverageMap (TestCoverageMap (..), readTestCoverageMapFile, writeTestCoverageMapFile)
import Test.Syd.Mutation.TestId (TestId, parseTestIdFilterArg, renderTestId)
import Test.Syd.OptParse
import Test.Syd.Output (printOutputSpecForest)
import Test.Syd.Run
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef

-- | Parent process: enumerate all leaf tests, spawn one coverage child
-- subprocess per test (up to N concurrently, N = 'getNumCapabilities'),
-- merge the resulting 'TestCoverageMap's, and write (or merge into)
-- @manifest-augmented.json@ in @settingMutationAugmentedManifestDir@.
--
-- When @settingMutationSuiteName@ is set, covering tests are recorded under
-- that suite name key.  If @manifest-augmented.json@ already exists (from a
-- prior suite's coverage pass), this run's results are merged in so that
-- multiple suites can be run sequentially.
runCoverageMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runCoverageMode settings manifestDirs spec = do
  specForest <- execTestDefM settings spec
  let leafIds = map fst (flattenTestForestWithIds specForest)
      total = length leafIds
  defaultExe <- getExecutablePath
  n <- getNumCapabilities
  sem <- newQSem n
  coverageMaps <-
    mapConcurrently
      (runCoverageChild defaultExe settings manifestDirs sem total)
      (zip [1 :: Int ..] leafIds)
  let TestCoverageMap coverageMap = mconcat coverageMaps
      mutationCoverage = invertCoverageMap coverageMap
      suiteName = fromMaybe "" (settingMutationSuiteName settings)
  allRecords <- mconcat <$> mapM readManifestDir manifestDirs
  let newAugmented = buildAugmentedManifest suiteName mutationCoverage allRecords
  augDir <- resolveAugmentedManifestDir settings
  existing <- readAugmentedManifestFileIfExists augDir
  let augmented = case existing of
        Nothing -> newAugmented
        Just prev -> mergeAugmentedManifests prev newAugmented
  writeAugmentedManifestFile augDir augmented
  where
    runCoverageChild defaultExe settings' manifestDirs' sem total (i, tid) =
      bracket_ (waitQSem sem) (signalQSem sem) $
        withSystemTempDir "coverage-child" $ \tmpDir -> do
          let outputFile = fromAbsFile (tmpDir </> [relfile|coverage.json|])
              -- Pass at least one --mutation-coverage dir so the child
              -- dispatches to runSingleCoverageMode rather than the normal
              -- test runner. The child ignores the manifest content; only
              -- the flag's presence matters for dispatch.
              coverageDirArgs = concatMap (\d -> ["--mutation-coverage", fromAbsDir d]) manifestDirs'
              args =
                coverageDirArgs
                  ++ [ "--mutation-coverage-one",
                       T.unpack (renderTestId tid),
                       "--mutation-coverage-output",
                       outputFile
                     ]
                  ++ case settingMutationSuiteName settings' of
                    Nothing -> []
                    Just name -> ["--mutation-suite-name", T.unpack name]
              childProc = proc defaultExe args
          ec <- runProcess childProc
          case ec of
            ExitFailure code ->
              fail $
                "coverage child for "
                  ++ T.unpack (renderTestId tid)
                  ++ " exited with code "
                  ++ show code
            ExitSuccess -> do
              mMap <- readTestCoverageMapFile outputFile
              case mMap of
                Nothing ->
                  fail $
                    "coverage child for "
                      ++ T.unpack (renderTestId tid)
                      ++ " wrote an unreadable coverage map"
                Just coverageMap -> do
                  let TestCoverageMap m = coverageMap
                      covered = fromMaybe Set.empty (Map.lookup tid m)
                  putStrLn $
                    "coverage ("
                      ++ show i
                      ++ "/"
                      ++ show total
                      ++ "): "
                      ++ T.unpack (renderTestId tid)
                      ++ " ("
                      ++ show (Set.size covered)
                      ++ " mutations)"
                  pure coverageMap

    buildAugmentedManifest suiteName mutationCoverage (MutationManifest records) =
      AugmentedManifest $
        concatMap (annotateRecord suiteName mutationCoverage) records

    annotateRecord suiteName mutationCoverage rec =
      let coveringTests =
            Set.toList $
              Map.findWithDefault Set.empty (mutRecId rec) mutationCoverage
          annotated =
            rec
              { mutRecCoveringTests =
                  Just $ Map.singleton suiteName coveringTests
              }
       in case fromMutationRecord annotated of
            Nothing -> []
            Just r -> [r]

-- | Invert a @'Map' 'TestId' ('Set' 'MutationId')@ to
-- @'Map' 'MutationId' ('Set' 'TestId')@.
invertCoverageMap :: Map.Map TestId (Set.Set MutationId) -> Map.Map MutationId (Set.Set TestId)
invertCoverageMap =
  Map.foldlWithKey'
    ( \acc tid mids ->
        Set.foldl'
          (\a mid -> Map.insertWith Set.union mid (Set.singleton tid) a)
          acc
          mids
    )
    Map.empty

-- | Child process: run the single test identified by @--mutation-coverage-one@,
-- write its 'TestCoverageMap' to @--mutation-coverage-output@, and exit.
runSingleCoverageMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runSingleCoverageMode settings _manifestDirs spec = do
  tid <- case settingMutationCoverageOne settings >>= parseTestIdFilterArg of
    Nothing -> fail "runSingleCoverageMode: no valid --mutation-coverage-one id"
    Just t -> pure t
  outputFile <- case settingMutationCoverageOutput settings of
    Nothing -> fail "runSingleCoverageMode: no --mutation-coverage-output file"
    Just f -> pure f
  specForest <- execTestDefM settings spec
  let coverageSettings =
        settings
          { settingThreads = Synchronous,
            settingMaxSuccess = 1
          }
      trie = testIdTrieFromList [tid]
      filtered = filterTestForestByTrie trie specForest
  ref <- newIORef Set.empty
  _ <- withCoverageSlot ref $ runSpecForestSynchronously coverageSettings filtered
  covered <- readIORef ref
  let coverageMap = TestCoverageMap (Map.singleton tid covered)
  writeTestCoverageMapFile outputFile coverageMap

-- | Resolve the augmented manifest directory from settings,
-- falling back to the current working directory.
resolveAugmentedManifestDir :: Settings -> IO (Path Abs Dir)
resolveAugmentedManifestDir settings =
  maybe getCurrentDir pure (settingMutationAugmentedManifestDir settings)

data MutationResult
  = MutationUncovered UncoveredMutation
  | MutationKilled
  | MutationSurvived (Maybe SurvivedMutation)

-- | Parent process: read @manifest-augmented.json@ and spawn one child
-- subprocess per mutation per suite that covers it.
--
-- Each child receives @--mutation <dir> --mutation-one <id>
-- --mutation-suite-name <suite> --mutation-augmented-manifest-dir <dir>@
-- and exits 0 (survived) or non-zero (killed).
--
-- Suites with no covering tests for a given mutation are skipped — running a
-- suite with an empty filter would cause sydtest to run all tests.
--
-- Prints @Killed: N@, @Survived: M@, and @Uncovered: K@ so the Nix report
-- derivation can parse them.  Also writes @report.json@ to
-- 'settingMutationReportDir' when set.
runMutationMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runMutationMode settings _manifestDirs _spec = do
  hSetBuffering stderr (BlockBuffering Nothing)
  augDir <- resolveAugmentedManifestDir settings
  AugmentedManifest records <- readAugmentedManifestFile augDir
  defaultExe <- getExecutablePath
  n <- getNumCapabilities
  sem <- newQSem n
  results <- mapConcurrently (runOne defaultExe augDir sem) records
  let (killed, survived, survivors, uncoveredMutations) = foldr tally (0, 0, [], []) results
      uncovered = length uncoveredMutations
      jsonReport =
        MutationRunReport
          { mutationRunReportKilled = killed,
            mutationRunReportSurvived = survived,
            mutationRunReportUncovered = uncovered,
            mutationRunReportSurvivors = survivors,
            mutationRunReportUncoveredMutations = uncoveredMutations
          }
  mapM_ (`writeMutationRunReport` jsonReport) (settingMutationReportDir settings)
  putStr $ renderMutationRunReport jsonReport
  where
    tally (MutationUncovered um) (k, s, ss, us) = (k, s, ss, us ++ [um])
    tally MutationKilled (k, s, ss, us) = (k + 1, s, ss, us)
    tally (MutationSurvived Nothing) (k, s, ss, us) = (k, s + 1, ss, us)
    tally (MutationSurvived (Just survivor)) (k, s, ss, us) = (k, s + 1, ss ++ [survivor], us)

    runOne defaultExe augDir sem record =
      bracket_ (waitQSem sem) (signalQSem sem) $ do
        let mid = augmentedMutationRecordId record
        hPutStr stderr $ renderMutationProgressEvent (MutationProgressEvent record)
        -- Only run suites that have at least one covering test for this mutation.
        let coveringBySuite =
              Map.filter (not . null) (augmentedMutationRecordCoveringTests record)
        if Map.null coveringBySuite
          then pure (MutationUncovered (UncoveredMutation record))
          else do
            -- Run one child per covering suite; mutation is killed if any child fails.
            outcomes <- mapM (runOneSuite defaultExe augDir mid) (Map.keys coveringBySuite)
            pure $ case [() | Left () <- outcomes] of
              (_ : _) -> MutationKilled
              [] ->
                MutationSurvived $
                  let mRelFile = listToMaybe [rf | Right (Just rf) <- outcomes]
                   in fmap (\relFile -> SurvivedMutation {survivedMutationRecord = record, survivedMutationLogFile = relFile}) mRelFile

    -- Returns Left () when the mutation was killed (child exited non-zero),
    -- Right (Just relFile) when survived and a log file was saved,
    -- Right Nothing when survived but no log file was saved.
    runOneSuite defaultExe augDir mid suiteName = do
      let suiteExes = settingMutationSuiteExes settings
          exe = fromMaybe defaultExe (Map.lookup suiteName suiteExes)
          rtsArgs = case settingMutationChildMemLimit settings of
            Nothing -> []
            Just limit -> ["+RTS", "-M" ++ limit, "-RTS"]
          suiteNameStr = T.unpack suiteName
          args =
            [ "--mutation",
              fromAbsDir augDir,
              "--mutation-one",
              renderMutationId mid,
              "--mutation-augmented-manifest-dir",
              fromAbsDir augDir,
              "--mutation-suite-name",
              suiteNameStr
            ]
              ++ rtsArgs
      withSystemTempDir "mutation-child" $ \tmpDir -> do
        let logPath = tmpDir </> [relfile|child.log|]
        logHandle <- openFile (fromAbsFile logPath) WriteMode
        let childProc =
              setStdout (useHandleOpen logHandle) $
                setStderr (useHandleOpen logHandle) $
                  proc exe args
        ec <- runProcess childProc
        hClose logHandle
        case ec of
          ExitFailure _ -> pure (Left ())
          ExitSuccess -> do
            mRelFile <- case settingMutationReportDir settings of
              Nothing -> pure Nothing
              Just reportDir -> do
                let logName =
                      "survivor-"
                        ++ map (\c -> if c == '/' then '-' else c) (renderMutationId mid)
                        ++ ( if T.null suiteName
                               then ""
                               else "-" ++ suiteNameStr
                           )
                        ++ ".log"
                case parseRelFile logName of
                  Nothing -> pure Nothing
                  Just relFile -> do
                    copyFile logPath (reportDir </> relFile)
                    pure (Just relFile)
            pure (Right mRelFile)

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
