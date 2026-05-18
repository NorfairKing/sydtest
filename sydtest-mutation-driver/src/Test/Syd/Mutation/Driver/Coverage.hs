{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parent-side runner for the coverage phase of mutation testing.
--
-- For each declared suite the driver invokes this runner once: it asks
-- the suite executable to enumerate its leaf tests, then spawns one
-- coverage child per test concurrently to collect coverage maps, and
-- merges the results into @manifest-augmented.json@.
module Test.Syd.Mutation.Driver.Coverage
  ( runCoverageMode,
  )
where

import Control.Concurrent (newQSem, signalQSem, waitQSem)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket_)
import qualified Control.Exception as Exception
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC.Conc (getNumCapabilities)
import Path
import Path.IO (withSystemTempDir)
import System.Exit (ExitCode (..), exitWith)
import System.IO (BufferMode (LineBuffering), hFlush, hSetBuffering, stderr)
import System.Process.Typed (proc, readProcessStdout_, runProcess)
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
    defaultTimeoutMicros,
    fromMutationRecord,
    mergeAugmentedManifests,
    readAugmentedManifestFileIfExists,
    writeAugmentedManifestFile,
  )
import Test.Syd.Mutation.Manifest
  ( MutationGroup (..),
    MutationManifest (..),
    MutationRecord (..),
    readManifestDir,
  )
import Test.Syd.Mutation.Runtime (MutationId)
import Test.Syd.Mutation.TestBaselineMap (TestBaselineMap (..), readTestBaselineMapFile)
import Test.Syd.Mutation.TestCoverageMap (TestCoverageMap (..), readTestCoverageMapFile)
import Test.Syd.Mutation.TestId (TestId, parseTestIdFilterArg, renderTestId)
import Test.Syd.MutationMode.Common
  ( CoverageFailFast (..),
    CoverageProgressEvent (..),
    CoverageProgressPhase (..),
    CoverageProgressSkipReason (..),
    CoverageProgressTestEvent (..),
    renderCoverageProgressEvent,
    resolveAugmentedManifestDir,
    retryingIO,
  )
import Text.Colour (TerminalCapabilities (..), chunk, fore, hPutChunksLocaleWith, red, unlinesChunks, yellow)

-- | Parent process: enumerate leaf tests by asking the suite executable
-- to list them, spawn one coverage child per test (up to @coverageJobs@
-- concurrently), merge the resulting 'TestCoverageMap's, and write or
-- merge into @manifest-augmented.json@.
--
-- When @manifest-augmented.json@ already exists (from a prior suite's
-- coverage pass), this run's results are merged in so that multiple
-- suites can be run sequentially.
runCoverageMode ::
  -- | Whether to abort the coverage run on a baseline test failure.
  Bool ->
  -- | Mutation manifest directories.
  [Path Abs Dir] ->
  -- | Optional augmented-manifest directory.  'Nothing' means CWD.
  Maybe (Path Abs Dir) ->
  -- | Maximum coverage-child concurrency.  'Nothing' means
  -- 'getNumCapabilities'.
  Maybe Int ->
  -- | Retry budget for a failing coverage child.
  Word ->
  -- | Name of this suite (used as the covering-tests key).
  Text ->
  -- | Path to the suite executable to invoke as a coverage child.
  Path Abs File ->
  IO ()
runCoverageMode failFast manifestDirs augmentedManifestDirM coverageJobs coverageRetry suiteName childExe = do
  -- LineBuffering on stderr so our writes hit the fd at line boundaries,
  -- not when the buffer happens to fill. Coverage children inherit this
  -- fd and write to it directly (bypassing our Handle's MVar lock); if
  -- our own bytes sit in a block buffer waiting to be flushed, they can
  -- interleave with whatever the children write in the meantime.
  hSetBuffering stderr LineBuffering
  allRecords@(MutationManifest groups) <- mconcat <$> mapM readManifestDir manifestDirs
  augDir <- resolveAugmentedManifestDir augmentedManifestDirM
  let writeEmptyAugmented = do
        existing <- readAugmentedManifestFileIfExists augDir
        writeAugmentedManifestFile augDir (fromMaybe (AugmentedManifest []) existing)
  if all (\(MutationGroup rs) -> null rs) groups
    then do
      emitCoverageEvent (CoverageProgressSkipped CoverageSkipNoMutations)
      writeEmptyAugmented
    else do
      leafIds <- listSuiteTestIds childExe
      let total = length leafIds
      if total == 0
        then do
          emitCoverageEvent (CoverageProgressSkipped CoverageSkipNoTests)
          writeEmptyAugmented
        else do
          n <- case coverageJobs of
            Just j | j > 0 -> pure j
            _ -> getNumCapabilities
          sem <- newQSem n
          -- A coverage child that observes a test failure (and was
          -- launched with --mutation-fail-fast) throws 'CoverageFailFast'
          -- from its worker thread.  'mapConcurrently' cancels the
          -- remaining workers and re-raises the exception, which we
          -- catch here so we can exit non-zero rather than crashing.
          childResults <-
            Exception.handle (\CoverageFailFast -> hFlush stderr >> exitWith (ExitFailure 1)) $
              mapConcurrently
                (runCoverageChild sem total)
                (zip [1 :: Int ..] leafIds)
          let (coverageMaps, baselineMaps) = unzip childResults
              TestCoverageMap coverageMap = mconcat coverageMaps
              TestBaselineMap baselineMap = mconcat baselineMaps
              mutationCoverage = invertCoverageMap coverageMap
          let newAugmented = buildAugmentedManifest mutationCoverage baselineMap allRecords
          existing <- readAugmentedManifestFileIfExists augDir
          let augmented = case existing of
                Nothing -> newAugmented
                Just prev -> mergeAugmentedManifests prev newAugmented
          writeAugmentedManifestFile augDir augmented
  where
    runCoverageChild sem total (i, tid) =
      bracket_ (waitQSem sem) (signalQSem sem) $ do
        emitCoverageEvent $
          CoverageProgressTest
            CoverageProgressTestEvent
              { coverageProgressIndex = i,
                coverageProgressTotal = total,
                coverageProgressTestId = tid,
                coverageProgressTestPhase = CoverageProgressStarting
              }
        result <- runCoverageChildAttempt tid coverageRetry
        let TestCoverageMap m = fst result
            covered = fromMaybe Set.empty (Map.lookup tid m)
        emitCoverageEvent $
          CoverageProgressTest
            CoverageProgressTestEvent
              { coverageProgressIndex = i,
                coverageProgressTotal = total,
                coverageProgressTestId = tid,
                coverageProgressTestPhase = CoverageProgressDone (Set.size covered)
              }
        pure result

    runCoverageChildAttempt tid retriesLeft = do
      result <- retryingIO retriesLeft (logCoverageRetry tid) (runOneCoverageChild tid)
      case result of
        Right v -> pure v
        Left reason ->
          fail $
            "coverage child for "
              ++ T.unpack (renderTestId tid)
              ++ ": "
              ++ reason

    -- One coverage-child attempt: returns @Left reason@ on transient
    -- failure (non-zero exit or unreadable output), @Right (cov, base)@
    -- on success.
    --
    -- Exit code 2 is reserved by the coverage child to mean "the test
    -- itself failed under fail-fast" — that is not a transient failure,
    -- so we throw 'CoverageFailFast' to abort the run without retrying.
    runOneCoverageChild tid =
      withSystemTempDir "coverage-child" $ \tmpDir -> do
        let outputFile = fromAbsFile (tmpDir </> [relfile|coverage.json|])
            baselineFile = fromAbsFile (tmpDir </> [relfile|baseline.json|])
            failFastArg =
              if failFast
                then "--mutation-fail-fast"
                else "--no-mutation-fail-fast"
            args =
              [ "--mutation-coverage-one",
                T.unpack (renderTestId tid),
                "--mutation-coverage-output",
                outputFile,
                "--mutation-coverage-baseline-output",
                baselineFile,
                failFastArg,
                "--mutation-suite-name",
                T.unpack suiteName
              ]
            childProc = proc (fromAbsFile childExe) args
        ec <- runProcess childProc
        case ec of
          ExitFailure 2 | failFast -> do
            hPutChunksLocaleWith With8BitColours stderr $
              unlinesChunks
                [ [ fore red (chunk "coverage: test failed during baseline run for "),
                    chunk (renderTestId tid),
                    chunk " — aborting (fail-fast)"
                  ]
                ]
            Exception.throwIO CoverageFailFast
          ExitFailure code -> pure $ Left ("exited with code " ++ show code)
          ExitSuccess -> do
            mMap <- readTestCoverageMapFile outputFile
            case mMap of
              Nothing -> pure $ Left "wrote an unreadable coverage map"
              Just coverageMap -> do
                mBaseline <- readTestBaselineMapFile baselineFile
                case mBaseline of
                  Nothing -> pure $ Left "wrote an unreadable baseline map"
                  Just baselineMap -> pure $ Right (coverageMap, baselineMap)

    logCoverageRetry tid reason retriesAfter =
      hPutChunksLocaleWith With8BitColours stderr $
        unlinesChunks
          [ [ fore yellow (chunk "coverage: retrying "),
              chunk (renderTestId tid),
              chunk " (",
              chunk (T.pack reason),
              chunk ", ",
              chunk (T.pack (show retriesAfter)),
              chunk " retr",
              chunk (if retriesAfter == 1 then "y" else "ies"),
              chunk " left)"
            ]
          ]

    buildAugmentedManifest mutationCoverage baselineMap (MutationManifest mgroups) =
      AugmentedManifest
        [ AugmentedMutationGroup augmented
        | MutationGroup recs <- mgroups,
          let augmented = concatMap (annotateRecord mutationCoverage baselineMap) recs,
          not (null augmented)
        ]

    annotateRecord mutationCoverage baselineMap rec =
      let coveringTests =
            Set.toList $
              Map.findWithDefault Set.empty (mutRecId rec) mutationCoverage
          -- Per-mutation timeout (microseconds) = 10 * sum of covering-test
          -- baselines, floored at 30s.  Tests with no recorded baseline
          -- contribute 0; the floor still applies.
          coveringBaselineSum :: Word
          coveringBaselineSum =
            sum [Map.findWithDefault 0 t baselineMap | t <- coveringTests]
          timeoutMicros = max defaultTimeoutMicros (10 * coveringBaselineSum)
          annotated =
            rec
              { mutRecCoveringTests =
                  Just $ Map.singleton suiteName coveringTests
              }
       in case fromMutationRecord annotated of
            Nothing -> []
            Just r ->
              [r {augmentedMutationRecordTimeoutMicros = timeoutMicros}]

emitCoverageEvent :: CoverageProgressEvent -> IO ()
emitCoverageEvent ev =
  hPutChunksLocaleWith With8BitColours stderr (unlinesChunks (renderCoverageProgressEvent ev))

-- | Ask the suite executable to print its leaf test IDs (one per line)
-- and parse the result.  The driver uses the @--mutation-coverage-list@
-- flag for this.
listSuiteTestIds :: Path Abs File -> IO [TestId]
listSuiteTestIds childExe = do
  let childProc = proc (fromAbsFile childExe) ["--mutation-coverage-list"]
  output <- readProcessStdout_ childProc
  let stripBOM t = case T.uncons t of
        Just ('\xFEFF', rest) -> rest
        _ -> t
      txt = stripBOM (decodeUtf8Lenient (LB.toStrict output))
      raw = filter (not . T.null) (T.lines txt)
  case mapM parseTestIdFilterArg raw of
    Just tids -> pure tids
    Nothing ->
      fail $
        "sydtest-mutation-driver: failed to parse test ids from "
          ++ fromAbsFile childExe
          ++ "'s --mutation-coverage-list output"

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
