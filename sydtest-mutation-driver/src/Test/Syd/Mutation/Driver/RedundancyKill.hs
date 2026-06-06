{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parent-side runner for the @kill@ basis of the @redundancy@ subcommand.
--
-- Builds the per-test kill matrix by spawning one kill-row child per mutation
-- per covering suite (the same run count as the mutation phase, /not/ one per
-- (test, mutation) pair): each child runs that mutation's covering tests with
-- the mutation active and reports each test's pass\/fail via a 'TestKillRow'.
-- The rows are aggregated into the per-suite @test -> killed-mutations@
-- relation, restricted to mutations that some test actually catches, and fed to
-- 'analyzeRedundancy'.
module Test.Syd.Mutation.Driver.RedundancyKill
  ( runKillRedundancy,
    buildKillRelations,
  )
where

import Control.Concurrent (QSem, newQSem, signalQSem, threadDelay, waitQSem)
import Control.Concurrent.Async (mapConcurrently, race)
import Control.Exception (bracket, bracket_)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Conc (getNumCapabilities)
import Path
import Path.IO (ignoringAbsence, withSystemTempDir)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), IOMode (..), hPutStrLn, hSetBuffering, stderr, withFile)
import System.Process.Typed
  ( ProcessConfig,
    proc,
    setStderr,
    setStdout,
    setWorkingDir,
    startProcess,
    stopProcess,
    useHandleOpen,
    waitExitCode,
  )
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
    mergeAugmentedManifests,
    readAugmentedManifestFile,
    writeAugmentedManifestFile,
  )
import Test.Syd.Mutation.Driver.OptParse (RedundancySettings (..), SuiteConfig (..))
import Test.Syd.Mutation.Driver.SuitePkg (walkSuitePkgs)
import Test.Syd.Mutation.KillRow (TestKillRow (..), readTestKillRowFile)
import Test.Syd.Mutation.Redundancy (RedundancyBasis (..), RedundancyReport, analyzeRedundancy)
import Test.Syd.Mutation.Runtime (MutationId, renderMutationId)
import Test.Syd.Mutation.TestId (TestId)

-- | Run the kill-matrix and return one 'RedundancyReport' per runnable suite.
runKillRedundancy :: RedundancySettings -> IO [RedundancyReport]
runKillRedundancy RedundancySettings {redundancySettingSuitePkgs, redundancySettingCoverageDirs, redundancySettingChildMemLimit} = do
  hSetBuffering stderr LineBuffering
  suites <- walkSuitePkgs redundancySettingSuitePkgs
  manifests <-
    mapM
      (\dir -> readAugmentedManifestFile (dir </> [reldir|augmented|]))
      redundancySettingCoverageDirs
  let AugmentedManifest groups = foldl' mergeAugmentedManifests (AugmentedManifest []) manifests
      records = [r | AugmentedMutationGroup rs <- groups, r <- rs]
      -- One work item per (mutation, covering suite) whose suite has an exe and
      -- a non-empty covering list.  A suite we cannot run is omitted entirely
      -- (its tests are neither analysed nor reported as removable).
      workItems =
        [ (record, suiteName, suiteConfig)
        | record <- records,
          (suiteName, coveringTids) <- Map.toList (augmentedMutationRecordCoveringTests record),
          not (null coveringTids),
          Just suiteConfig <- [Map.lookup suiteName suites]
        ]
  withSystemTempDir "redundancy-kill-augmented" $ \augDir -> do
    writeAugmentedManifestFile augDir (AugmentedManifest groups)
    n <- getNumCapabilities
    sem <- newQSem n
    rows <-
      catMaybes
        <$> mapConcurrently
          (runKillRowChild sem augDir redundancySettingChildMemLimit)
          workItems
    let runnableSuites = Map.keysSet suites
        relBySuite = buildKillRelations runnableSuites records rows
    pure
      [ analyzeRedundancy BasisKill suite Map.empty rel
      | (suite, rel) <- Map.toList relBySuite
      ]

-- | Aggregate the kill matrix into the per-suite @test -> killed-mutations@
-- relation.  Seeded with every covering test mapped to the empty set (so a test
-- that catches nothing still appears, as removable), then overlaid with the
-- mutations each test actually killed.  Restricted to the given runnable suites.
buildKillRelations ::
  Set Text ->
  [AugmentedMutationRecord] ->
  [(Text, MutationId, TestKillRow)] ->
  Map.Map Text (Map.Map TestId (Set MutationId))
buildKillRelations runnableSuites records rows =
  Map.unionWith (Map.unionWith Set.union) seedRel killsRel
  where
    seedRel =
      Map.fromListWith
        (Map.unionWith Set.union)
        [ (suite, Map.singleton tid Set.empty)
        | record <- records,
          (suite, tids) <- Map.toList (augmentedMutationRecordCoveringTests record),
          suite `Set.member` runnableSuites,
          tid <- tids
        ]
    killsRel =
      Map.fromListWith
        (Map.unionWith Set.union)
        [ (suite, Map.singleton tid (Set.singleton mid))
        | (suite, mid, TestKillRow killMap) <- rows,
          suite `Set.member` runnableSuites,
          (tid, killed) <- Map.toList killMap,
          killed
        ]

-- | Spawn one kill-row child for a (mutation, suite) pair and read back its
-- 'TestKillRow'.  Returns 'Nothing' (with a stderr note) on timeout, non-zero
-- exit, or an unreadable row, so a single bad mutation does not sink the run.
runKillRowChild ::
  -- | Concurrency semaphore.
  QSem ->
  -- | Augmented-manifest directory the child reads covering tests from.
  Path Abs Dir ->
  -- | Optional RTS heap cap.
  Maybe String ->
  (AugmentedMutationRecord, Text, SuiteConfig) ->
  IO (Maybe (Text, MutationId, TestKillRow))
runKillRowChild sem augDir memLimit (record, suiteName, SuiteConfig {suiteConfigExe, suiteConfigResourceDir}) =
  bracket_ (waitQSem sem) (signalQSem sem) $
    withSystemTempDir "redundancy-kill-child" $ \tmpDir -> do
      let mid = augmentedMutationRecordId record
          outFile = tmpDir </> [relfile|killrow.json|]
          logPath = tmpDir </> [relfile|child.log|]
          rtsArgs = case memLimit of
            Nothing -> []
            Just limit -> ["+RTS", "-M" ++ limit, "-RTS"]
          args =
            [ "--mutation-kill-row-one",
              renderMutationId mid,
              "--mutation-augmented-manifest-dir",
              fromAbsDir augDir,
              "--mutation-suite-name",
              T.unpack suiteName,
              "--mutation-kill-row-output",
              fromAbsFile outFile
            ]
              ++ rtsArgs
          timeoutMicros = augmentedMutationRecordTimeoutMicros record
          micros =
            if timeoutMicros >= fromIntegral (maxBound :: Int)
              then maxBound :: Int
              else fromIntegral timeoutMicros
      outcome <-
        withFile (fromAbsFile logPath) WriteMode $ \logHandle -> do
          let childProc =
                maybe
                  id
                  (\rd -> setWorkingDir (fromAbsDir rd))
                  suiteConfigResourceDir
                  $ setStdout (useHandleOpen logHandle)
                  $ setStderr (useHandleOpen logHandle)
                  $ proc (fromAbsFile suiteConfigExe) args
          startProcessAndWait childProc micros
      case outcome of
        Left () -> do
          note mid suiteName "timed out"
          pure Nothing
        Right (ExitFailure code) -> do
          note mid suiteName ("child exited with code " ++ show code)
          pure Nothing
        Right ExitSuccess -> do
          eRow <- readTestKillRowFile (fromAbsFile outFile)
          case eRow of
            Left err -> do
              note mid suiteName ("unreadable kill row: " ++ err)
              pure Nothing
            Right row -> pure (Just (suiteName, mid, row))
  where
    note mid suite reason =
      hPutStrLn stderr $
        "redundancy (kill): skipping "
          ++ renderMutationId mid
          ++ (if T.null suite then "" else " [" ++ T.unpack suite ++ "]")
          ++ ": "
          ++ reason

-- | Race a child against a timeout; on timeout, stop it (and report 'Left').
-- 'bracket' guarantees the child is reaped on every path (mirrors the mutation
-- phase's runner).
startProcessAndWait ::
  ProcessConfig stdin stdout stderrType ->
  Int ->
  IO (Either () ExitCode)
startProcessAndWait childProc micros =
  bracket (startProcess childProc) (ignoringAbsence . stopProcess) $ \p -> do
    result <- race (threadDelay micros) (waitExitCode p)
    case result of
      Left () -> pure (Left ())
      Right ec -> pure (Right ec)
