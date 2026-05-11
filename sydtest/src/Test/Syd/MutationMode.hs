{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.MutationMode (runMutationMode, runCoverageMode, formatMutationLog) where

import Control.Exception (Handler (..), SomeAsyncException, SomeException, bracket_, catches, throwIO)
import Data.IORef
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import Path
import System.IO (hPutStrLn, stderr)
import Test.Syd.Def
import Test.Syd.Mutation.Forest (filterTestForestByTrie, flattenTestForestWithIds, testIdTrieFromList)
import Test.Syd.Mutation.Manifest
  ( MutationManifest (..),
    MutationRecord (..),
    readCoverageDir,
    readManifestDir,
    writeCoverageFile,
  )
import Test.Syd.Mutation.Runtime (MutationId (..), setActiveMutation, withCoverageSlot)
import Test.Syd.Mutation.TestId (TestId)
import Test.Syd.OptParse
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
-- @.coverage.json@ files alongside the existing manifest files.
--
-- For each manifest directory, reads the plugin-written @*.json@ files,
-- runs every test once (no mutation active) to record which 'MutationId's
-- each test reaches, then writes @<module>.coverage.json@ with
-- 'mutRecCoveringTests' filled in.
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
  mapM_ (writeCoverageForDir mutationCoverage) manifestDirs
  where
    writeCoverageForDir mutationCoverage manifestDir = do
      MutationManifest records <- readManifestDir manifestDir
      -- Group records by module name (first component of MutationId).
      let byModule = Map.fromListWith (++) [(moduleName r, [r]) | r <- records]
      mapM_ (writeModuleCoverage mutationCoverage manifestDir) (Map.toList byModule)

    moduleName MutationRecord {mutRecId = MutationId parts} = case parts of
      (mn : _) -> mn
      [] -> "Unknown"

    writeModuleCoverage mutationCoverage manifestDir (mn, records) = do
      let annotated = map (annotateCoverage mutationCoverage) records
      writeCoverageFile manifestDir mn (MutationManifest annotated)

    annotateCoverage mutationCoverage rec =
      rec
        { mutRecCoveringTests =
            Just $
              Set.toList $
                Map.findWithDefault Set.empty (mutRecId rec) mutationCoverage
        }

-- | Run the spec once per mutation in the manifest directories, in-process.
--
-- For each mutation, if coverage data is available (via @.coverage.json@ files),
-- only the covering tests are run.  Otherwise the full suite is run.
--
-- Prints @Killed: N@, @Survived: M@, and @Uncovered: K@ so the Nix report
-- derivation can parse them.
runMutationMode :: Settings -> [Path Abs Dir] -> Spec -> IO ()
runMutationMode settings manifestDirs spec = do
  MutationManifest records <- mconcat <$> mapM readManifestDir manifestDirs
  MutationManifest coverageRecords <- mconcat <$> mapM readCoverageDir manifestDirs
  let recordMap = Map.fromList [(mutRecId r, r) | r <- records]
      -- Coverage records override the base records' coveringTests field.
      coverageMap = Map.fromList [(mutRecId r, mutRecCoveringTests r) | r <- coverageRecords]
      mutations = map mutRecId records
  -- [check] The forest is built once and reused across mutations. Test bodies
  -- (IO actions) are re-executed each run, so ifMutation's NOINLINE protects
  -- them. However, any values computed via runIO during spec construction are
  -- memoized and will not reflect the active mutation. If that causes incorrect
  -- results, move execTestDefM inside runOne so the spec is re-evaluated per mutation.
  specForest <- execTestDefM settings spec
  (killed, survived, uncovered) <-
    foldl (runOne recordMap coverageMap specForest) (pure (0 :: Int, 0 :: Int, 0 :: Int)) mutations
  putStrLn $ "Killed: " ++ show killed
  putStrLn $ "Survived: " ++ show survived
  putStrLn $ "Uncovered: " ++ show uncovered
  where
    mutationSettings = settings {settingThreads = Synchronous}

    -- Run @forest@ with @mid@ active. Returns True if the mutation was killed
    -- (test failure or exception), False if it survived.
    -- Async exceptions are re-thrown; all synchronous exceptions count as kills.
    runMutation forest mid =
      bracket_ (setActiveMutation (Just mid)) (setActiveMutation Nothing) $
        catches
          ( do
              timedResult <- runSpecForestSynchronously mutationSettings forest
              pure (shouldExitFail mutationSettings (timedValue timedResult))
          )
          [ Handler (\e -> throwIO (e :: SomeAsyncException)),
            Handler
              ( \e -> do
                  hPutStrLn stderr $ "mutation: exception during test run: " ++ show (e :: SomeException)
                  pure True
              )
          ]

    runOne recordMap coverageMap specForest accIO mid = do
      (killed, survived, uncovered) <- accIO
      hPutStrLn stderr $ formatMutationLog mid (Map.lookup mid recordMap)
      case Map.lookup mid coverageMap of
        -- No coverage data: run the full suite.
        Nothing -> do
          killed' <- runMutation specForest mid
          if killed' then pure (killed + 1, survived, uncovered) else pure (killed, survived + 1, uncovered)
        -- Coverage data present but empty: mutation is uncovered.
        Just (Just []) ->
          pure (killed, survived, uncovered + 1)
        -- Coverage data with tests: run only the covering tests.
        Just (Just coveringTests) -> do
          let trie = testIdTrieFromList coveringTests
              filtered = filterTestForestByTrie trie specForest
          killed' <- runMutation filtered mid
          if killed' then pure (killed + 1, survived, uncovered) else pure (killed, survived + 1, uncovered)
        -- Coverage field is Nothing (not yet collected): run the full suite.
        Just Nothing -> do
          killed' <- runMutation specForest mid
          if killed' then pure (killed + 1, survived, uncovered) else pure (killed, survived + 1, uncovered)

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
