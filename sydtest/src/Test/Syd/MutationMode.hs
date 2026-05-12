{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module provides three entry points for mutation testing:
--
-- * 'runCoverageMode': collect per-test coverage and write @manifest-augmented.json@.
-- * 'runMutationMode': parent process — read the augmented manifest and spawn
--   one child process per mutation.
-- * 'runSingleMutationMode': child process — run only the tests that cover a
--   single mutation and exit with success/failure.
--
-- Running each mutation in a subprocess is necessary for three reasons:
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
-- 3. __Parallelism__: because 'activeMutation' is a process-global 'IORef',
--    mutations must run serially within a single process.  Subprocesses are
--    independent and can be run in parallel (disjoint mutation sets) in a
--    future extension without any changes to the child or the manifest format.
module Test.Syd.MutationMode
  ( runMutationMode,
    runSingleMutationMode,
    runCoverageMode,
    formatMutationLog,
  )
where

import Data.IORef
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Path
import Path.IO (copyFile, getCurrentDir, withSystemTempFile)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (proc, runProcess, setStderr, setStdout, useHandleOpen)
import Test.Syd.Def
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationRecord (..),
    MutationRunReport (..),
    SurvivedMutation (..),
    fromMutationRecord,
    lookupAugmentedMutationRecord,
    readAugmentedManifestFile,
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
import Test.Syd.Mutation.TestId (TestId)
import Test.Syd.OptParse
import Test.Syd.Output (printOutputSpecForest)
import Test.Syd.Run
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef

-- | Run every test in @forest@ once (no active mutation) and record which
-- 'MutationId's each test's execution reaches via 'ifMutation'.
--
-- Returns a map from each 'TestId' to the set of 'MutationId's it covers.
collectCoverage :: Settings -> TestForest '[] () -> IO (Map.Map TestId (Set.Set MutationId))
collectCoverage settings forest = do
  let coverageSettings =
        settings
          { settingThreads = Synchronous,
            -- One iteration is enough to discover which mutation sites a test
            -- reaches; running the full QuickCheck suite would be wasteful.
            settingMaxSuccess = 1
          }
      leafIds = map fst (flattenTestForestWithIds forest)
  Map.fromList <$> mapM (collectOne coverageSettings forest) leafIds
  where
    collectOne coverageSettings forest' tid = do
      let trie = testIdTrieFromList [tid]
          filtered = filterTestForestByTrie trie forest'
      ref <- newIORef Set.empty
      _ <-
        withCoverageSlot ref $
          runSpecForestSynchronously coverageSettings filtered
      covered <- readIORef ref
      pure (tid, covered)

-- | Collect per-test coverage for the mutations in @manifestDirs@ and write
-- @manifest-augmented.json@ to @settingMutationAugmentedManifestDir@.
--
-- For each manifest directory, reads the plugin-written @*.json@ files,
-- runs every test once (no mutation active) to record which 'MutationId's
-- each test reaches, then writes the augmented manifest with
-- 'augmentedMutationRecordCoveringTests' filled in.
runCoverageMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runCoverageMode settings manifestDirs spec = do
  specForest <- execTestDefM settings spec
  coverageMap <- collectCoverage settings specForest
  -- Invert: Map TestId (Set MutationId) -> Map MutationId (Set TestId)
  let mutationCoverage =
        Map.foldlWithKey'
          ( \acc tid mids ->
              Set.foldl'
                (\a mid -> Map.insertWith Set.union mid (Set.singleton tid) a)
                acc
                mids
          )
          Map.empty
          coverageMap
  allRecords <- mconcat <$> mapM readManifestDir manifestDirs
  let augmented = buildAugmentedManifest mutationCoverage allRecords
  augDir <- resolveAugmentedManifestDir settings
  writeAugmentedManifestFile augDir augmented
  where
    buildAugmentedManifest mutationCoverage (MutationManifest records) =
      AugmentedManifest $
        concatMap (annotateRecord mutationCoverage) records

    annotateRecord mutationCoverage rec =
      let annotated =
            rec
              { mutRecCoveringTests =
                  Just $
                    Set.toList $
                      Map.findWithDefault Set.empty (mutRecId rec) mutationCoverage
              }
       in case fromMutationRecord annotated of
            Nothing -> []
            Just r -> [r]

-- | Resolve the augmented manifest directory from settings,
-- falling back to the current working directory.
resolveAugmentedManifestDir :: Settings -> IO (Path Abs Dir)
resolveAugmentedManifestDir settings =
  maybe getCurrentDir pure (settingMutationAugmentedManifestDir settings)

-- | Parent process: read @manifest-augmented.json@ and spawn one child
-- subprocess per mutation.
--
-- The child receives @--mutation <dir> --mutation-one <id>
-- --mutation-augmented-manifest-dir <dir>@ and exits 0 (survived) or
-- non-zero (killed).
--
-- Prints @Killed: N@, @Survived: M@, and @Uncovered: K@ so the Nix report
-- derivation can parse them.  Also writes @report.json@ to
-- 'settingMutationReportDir' when set.
runMutationMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runMutationMode settings _manifestDirs _spec = do
  augDir <- resolveAugmentedManifestDir settings
  AugmentedManifest records <- readAugmentedManifestFile augDir
  exe <- getExecutablePath
  (killed, survived, uncovered, survivors) <-
    foldl (runOne exe augDir) (pure (0 :: Int, 0 :: Int, 0 :: Int, [])) records
  let jsonReport =
        MutationRunReport
          { mutationRunReportKilled = killed,
            mutationRunReportSurvived = survived,
            mutationRunReportUncovered = uncovered,
            mutationRunReportSurvivors = reverse survivors
          }
  mapM_ (`writeMutationRunReport` jsonReport) (settingMutationReportDir settings)
  putStrLn $ "Killed: " ++ show killed
  putStrLn $ "Survived: " ++ show survived
  putStrLn $ "Uncovered: " ++ show uncovered
  where
    runOne exe augDir accIO record = do
      (killed, survived, uncovered, survivors) <- accIO
      let mid = augmentedMutationRecordId record
      hPutStrLn stderr $ formatMutationLog mid (Just (toMutationRecord record))
      case augmentedMutationRecordCoveringTests record of
        [] -> pure (killed, survived, uncovered + 1, survivors)
        _ -> do
          let rtsArgs = case settingMutationChildMemLimit settings of
                Nothing -> []
                Just limit -> ["+RTS", "-M" ++ limit, "-RTS"]
              args =
                [ "--mutation",
                  fromAbsDir augDir,
                  "--mutation-one",
                  renderMutationId mid,
                  "--mutation-augmented-manifest-dir",
                  fromAbsDir augDir
                ]
                  ++ rtsArgs
          (exitCode, mLogFile) <- withSystemTempFile "mutation-child.log" $ \logPath logHandle -> do
            -- useHandleOpen shares the handle with the child without closing it,
            -- so we can read the file after runProcess while still in the bracket.
            let childProc =
                  setStdout (useHandleOpen logHandle) $
                    setStderr (useHandleOpen logHandle) $
                      proc exe args
            ec <- runProcess childProc
            case ec of
              ExitFailure _ -> pure (ec, Nothing)
              ExitSuccess -> do
                output <- readFile (fromAbsFile logPath)
                putStr $ formatMutationLog mid (Just (toMutationRecord record))
                putStr output
                mRelFile <- case settingMutationReportDir settings of
                  Nothing -> pure Nothing
                  Just reportDir -> do
                    let logName = "survivor-" ++ map (\c -> if c == '/' then '-' else c) (renderMutationId mid) ++ ".log"
                    case parseRelFile logName of
                      Nothing -> pure Nothing
                      Just relFile -> do
                        copyFile logPath (reportDir </> relFile)
                        pure (Just relFile)
                pure (ec, mRelFile)
          case exitCode of
            ExitSuccess ->
              case mLogFile of
                Nothing -> pure (killed, survived + 1, uncovered, survivors)
                Just relFile ->
                  let survivor = SurvivedMutation {survivedMutationRecord = record, survivedMutationLogFile = relFile}
                   in pure (killed, survived + 1, uncovered, survivor : survivors)
            ExitFailure _ -> pure (killed + 1, survived, uncovered, survivors)

-- | Convert an 'AugmentedMutationRecord' back to a 'MutationRecord' for
-- display purposes.
toMutationRecord :: AugmentedMutationRecord -> MutationRecord
toMutationRecord AugmentedMutationRecord {augmentedMutationRecordId, augmentedMutationRecordOperator, augmentedMutationRecordOriginal, augmentedMutationRecordReplacement, augmentedMutationRecordSourceFile, augmentedMutationRecordSourceLine, augmentedMutationRecordMutatedLine, augmentedMutationRecordContextBefore, augmentedMutationRecordContextAfter, augmentedMutationRecordCoveringTests} =
  MutationRecord
    { mutRecId = augmentedMutationRecordId,
      mutRecOperator = augmentedMutationRecordOperator,
      mutRecOriginal = augmentedMutationRecordOriginal,
      mutRecReplacement = augmentedMutationRecordReplacement,
      mutRecSourceFile = augmentedMutationRecordSourceFile,
      mutRecSourceLine = augmentedMutationRecordSourceLine,
      mutRecMutatedLine = augmentedMutationRecordMutatedLine,
      mutRecContextBefore = augmentedMutationRecordContextBefore,
      mutRecContextAfter = augmentedMutationRecordContextAfter,
      mutRecCoveringTests = Just augmentedMutationRecordCoveringTests
    }

-- | Child process: run only the tests covering a single mutation and exit
-- with the appropriate exit code.
runSingleMutationMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runSingleMutationMode settings _manifestDirs spec = do
  mid <- case settingMutationOne settings >>= parseMutationId of
    Nothing -> fail "runSingleMutationMode: no valid --mutation-one id"
    Just m -> pure m
  augDir <- resolveAugmentedManifestDir settings
  augmented <- readAugmentedManifestFile augDir
  specForest <- execTestDefM settings spec
  let coveringTests =
        maybe
          []
          augmentedMutationRecordCoveringTests
          (lookupAugmentedMutationRecord mid augmented)
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

formatMutationLog :: MutationId -> Maybe MutationRecord -> String
formatMutationLog (MutationId parts) mRec =
  case (parts, mRec) of
    ( [modName, op, lineStr, colStartStr, colEndStr],
      Just MutationRecord {mutRecOriginal, mutRecReplacement, mutRecSourceFile, mutRecSourceLine, mutRecMutatedLine, mutRecContextBefore, mutRecContextAfter}
      ) ->
        let filePath = case mutRecSourceFile of
              Just p -> fromRelFile p
              Nothing -> moduleToFilePath modName
            header = "Testing mutation " ++ op ++ " at " ++ filePath ++ ":" ++ lineStr ++ ":" ++ colStartStr ++ "-" ++ colEndStr ++ ":"
         in case mutRecSourceLine of
              Nothing ->
                unlines
                  [ header,
                    "    - " ++ mutRecOriginal,
                    "    + " ++ mutRecReplacement
                  ]
              Just srcLine ->
                let lineNum = read lineStr :: Int
                    nBefore = length mutRecContextBefore
                    hunkHeader =
                      "@@ -"
                        ++ show (lineNum - nBefore)
                        ++ ","
                        ++ show (nBefore + 1 + length mutRecContextAfter)
                        ++ " +"
                        ++ show (lineNum - nBefore)
                        ++ ","
                        ++ show (nBefore + 1 + length mutRecContextAfter)
                        ++ " @@"
                    mutatedLine = fromMaybe srcLine mutRecMutatedLine
                 in T.unpack $
                      T.unlines $
                        map T.pack [header, hunkHeader]
                          ++ map (T.cons ' ') mutRecContextBefore
                          ++ [T.cons '-' srcLine, T.cons '+' mutatedLine]
                          ++ map (T.cons ' ') mutRecContextAfter
    _ ->
      "Testing mutation " ++ intercalate "/" parts
  where
    moduleToFilePath m = map (\c -> if c == '.' then '/' else c) m ++ ".hs"
