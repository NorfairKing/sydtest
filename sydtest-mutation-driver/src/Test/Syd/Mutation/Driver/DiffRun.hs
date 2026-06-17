{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parent-side runner for the @diff@ subcommand: the diff-scoped mutation
-- runner.
--
-- Reads the cached augmented manifest (the which-test-covers-which-mutation
-- map) and the cached per-suite @TestId -> source-location@ listings,
-- selects the subset of mutations implied by a unified diff, and runs only
-- those mutation children.  No compilation and no coverage phase happen here:
-- everything expensive was produced by the Nix build that cached these
-- artifacts.
module Test.Syd.Mutation.Driver.DiffRun
  ( runDiff,
    renderDiffSelection,
    renderDiffFinalSummary,
  )
where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import Path
import Path.IO (forgivingAbsence, withSystemTempDir)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hFlush, stderr, stdout)
import System.Process.Typed (proc, readProcessStdout_)
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationRecord (..),
    MutationGroupReport (..),
    MutationOutcome (..),
    MutationRunReport (..),
    SurvivedMutation (..),
    filterAugmentedManifestByIds,
    mergeAugmentedManifests,
    readAugmentedManifestFile,
    writeAugmentedManifestFile,
  )
import Test.Syd.Mutation.Driver.AssertScore (AssertScoreResult (..), assertScoreResult)
import Test.Syd.Mutation.Driver.Diff
  ( DiffSelectionBreakdown (..),
    parseUnifiedDiff,
    selectMutationsBreakdown,
  )
import Test.Syd.Mutation.Driver.Mutate (runMutationMode)
import Test.Syd.Mutation.Driver.OptParse
  ( DiffSettings (..),
    DiffSource (..),
  )
import Test.Syd.Mutation.Driver.SuitePkg (walkSuitePkgs)
import Test.Syd.Mutation.TestId (TestId)
import Test.Syd.Mutation.TestLocation (TestLocation (..), decodeTestLocations)
import Test.Syd.MutationMode.Common (formatMutationLog, survivorMitigationLines)
import Text.Colour
  ( Chunk,
    TerminalCapabilities (..),
    chunk,
    cyan,
    fore,
    green,
    hPutChunksLocaleWith,
    putChunksLocaleWith,
    red,
    unlinesChunks,
  )

-- | Run the diff subcommand: select and run only the diff-implied mutations.
runDiff :: DiffSettings -> IO ()
runDiff DiffSettings {..} = do
  -- 1. Obtain and parse the diff.
  diffText <- obtainDiff diffSettingSource
  hunks <- case parseUnifiedDiff diffText of
    Left err -> fail ("sydtest-mutation-driver diff: " ++ err)
    Right hs -> pure hs

  -- 2. Resolve the suite map (suite name -> exe + resource dir).
  suites <- walkSuitePkgs diffSettingSuitePkgs

  -- 3. Read the per-package coverage directories: union their augmented
  -- manifests in-memory, and read each suite's test-location listing from
  -- whichever directory provides it.  Consuming the per-package coverage
  -- directly avoids a separate merge derivation.
  manifests <-
    mapM
      (\dir -> readAugmentedManifestFile (dir </> [reldir|augmented|]))
      diffSettingCoverageDirs
  let manifest = foldl mergeAugmentedManifests (AugmentedManifest []) manifests
  testLocationsBySuite <-
    Map.traverseWithKey
      (\suiteName _ -> readTestLocations diffSettingCoverageDirs suiteName)
      suites

  -- 4. Select the diff-implied mutations and filter the manifest down to them.
  let breakdown = selectMutationsBreakdown hunks manifest testLocationsBySuite
      selected =
        Set.union
          (diffSelectionSourceMutations breakdown)
          (diffSelectionTestMutations breakdown)
      filtered = filterAugmentedManifestByIds selected manifest
      numSourceSelected = Set.size (diffSelectionSourceMutations breakdown)
      numTestSelected = Set.size (diffSelectionTestMutations breakdown)
      numSelected = Set.size selected

  hPutChunksLocaleWith With8BitColours stderr $
    unlinesChunks
      (renderDiffSelection (length hunks) numSourceSelected numTestSelected numSelected)

  -- 5. Run the mutation phase over the filtered manifest.  'runMutationMode'
  -- reads the augmented manifest from a directory, so write the filtered
  -- manifest to a temp dir and point it there.  It also writes
  -- report.txt/report.json into 'diffSettingOutDir' and prints the rendered
  -- body to stdout before returning, so the body is in the build log even
  -- for an empty selection.
  report <-
    withSystemTempDir "mutation-diff-augmented" $ \augDir -> do
      writeAugmentedManifestFile augDir filtered
      runMutationMode
        diffSettingFailFast
        diffSettingDebug
        augDir
        diffSettingOutDir
        diffSettingChildMemLimit
        diffSettingMutationJobs
        suites

  -- 6. Final summary block: PASS/FAIL header + report paths, written to
  -- stdout so they're the last thing in the CI build log.  Hard-code
  -- 'assertNoneUncovered = True' for the diff runner: an uncovered
  -- diff-touched mutation is a FAIL, matching the strictest mode of the
  -- full check.
  let assertion = assertScoreResult True report
      txtPath = diffSettingOutDir </> [relfile|report.txt|]
      jsonPath = diffSettingOutDir </> [relfile|report.json|]
      summary =
        renderDiffFinalSummary
          numSelected
          assertion
          report
          (T.pack (fromAbsFile txtPath))
          (T.pack (fromAbsFile jsonPath))
  -- 'runMutationMode' already flushed stderr; flush again so any further
  -- stderr writes from the runtime can't land after our stdout summary.
  hFlush stderr
  putChunksLocaleWith With8BitColours (unlinesChunks summary)
  hFlush stdout
  -- A 0-selection run always passes (we did no work); only exit non-zero
  -- when the underlying assertion failed.
  if numSelected > 0 && assertScoreFailed assertion
    then exitWith (ExitFailure 1)
    else pure ()

-- | Render the final summary block of the diff runner.  This is the
-- *last* thing written to stdout, so it's what the CI build log ends
-- with: a coloured PASS\/FAIL header followed by the report-file paths
-- (txt + json).
--
-- The header is special-cased for a 0-selection run: instead of "PASS:
-- All 0 mutation(s) accounted for." it reads "PASS: nothing to
-- mutation-test in this diff.", which is honest about why the run was a
-- no-op.  For a non-empty selection, the header comes from
-- 'assertScoreResult'.
--
-- When the run produced survivors, their full per-mutation detail (the
-- same diff blocks 'renderMutationRunReport' prints) is repeated here,
-- between the header and the report paths.  The full report body printed
-- earlier by 'runMutationMode' lists survivors before the uncovered and
-- skipped sections, so in a long CI log the survivors scroll off the
-- bottom; pinning a survivors-only block to the very end means the thing
-- that failed the run is the last thing the log shows.
--
-- Pure so it can be golden-tested without spinning up a real run; the
-- output is exactly the @stdout@ block the CI log ends with.
renderDiffFinalSummary ::
  -- | Number of mutations selected for the run.
  Int ->
  -- | Assertion result for the produced report.  Ignored when 0
  -- mutations were selected.
  AssertScoreResult ->
  -- | The produced report, for the survivors-only detail block.
  MutationRunReport ->
  -- | Path to @report.txt@ in the out dir.
  Text ->
  -- | Path to @report.json@ in the out dir.
  Text ->
  [[Chunk]]
renderDiffFinalSummary numSelected assertion report txtPath jsonPath =
  [ [],
    header,
    []
  ]
    ++ survivorsBlock
    ++ [ [chunk "Full report:             ", chunk txtPath],
         [chunk "Machine-readable report: ", chunk jsonPath]
       ]
  where
    header
      | numSelected == 0 =
          [fore green (chunk "PASS: "), chunk "nothing to mutation-test in this diff."]
      | otherwise = assertScoreHeader assertion
    survivors =
      [ s
      | g <- mutationRunReportGroups report,
        OutcomeSurvived s <- mutationGroupReportOutcomes g
      ]
    survivorsBlock
      | null survivors = []
      | otherwise =
          [[fore red (chunk "Surviving mutations:")]]
            ++ concatMap renderSurvivor survivors
            ++ [[]]
    renderSurvivor s =
      let rec = survivedMutationRecord s
       in ([] : formatMutationLog (augmentedMutationRecordId rec) rec)
            ++ survivorMitigationLines rec

-- | Render the one-line "N changed hunks; selected M mutations" progress
-- message as coloured chunks, matching the rest of the driver's output.
-- The breakdown ("X from source, Y from tests") makes a 0-selection run
-- self-explanatory.
renderDiffSelection :: Int -> Int -> Int -> Int -> [[Chunk]]
renderDiffSelection numHunks numSource numTest numTotal =
  [ [ chunk "diff: ",
      fore cyan (chunk (T.pack (show numHunks))),
      chunk (plural numHunks " changed hunk" " changed hunks"),
      chunk "; selected ",
      fore green (chunk (T.pack (show numTotal))),
      chunk (plural numTotal " mutation to run" " mutations to run"),
      chunk " (",
      chunk (T.pack (show numSource)),
      chunk " from source, ",
      chunk (T.pack (show numTest)),
      chunk " from tests)."
    ]
  ]
  where
    plural n one many = if n == 1 then one else many

-- | Obtain the unified diff text from the configured source.
obtainDiff :: DiffSource -> IO Text
obtainDiff = \case
  DiffSourceFile f -> decodeUtf8Lenient <$> SB.readFile (fromAbsFile f)
  DiffSourceStdin -> decodeUtf8Lenient . LB.toStrict <$> LB.getContents
  DiffSourceGitMergeBase base -> gitMergeBaseDiff base

-- | Compute @git diff <merge-base>@ where @<merge-base>@ is the merge-base of
-- @HEAD@ and the given base branch.  Run from the current working directory
-- (the wrapper @cd@s into the repo before invoking the driver).
gitMergeBaseDiff :: String -> IO Text
gitMergeBaseDiff base = do
  mergeBaseOut <-
    readProcessStdout_ (proc "git" ["merge-base", base, "HEAD"])
  let mergeBase = T.strip (decodeUtf8Lenient (LB.toStrict mergeBaseOut))
  diffOut <-
    readProcessStdout_ (proc "git" ["diff", T.unpack mergeBase])
  pure (decodeUtf8Lenient (LB.toStrict diffOut))

-- | Read a suite's @TestId -> (file, line)@ map from
-- @\<coverage-dir\>/test-locations/\<suite-name\>.json@, unioning the listing
-- from every per-package coverage directory that has one.  (A suite lives in
-- exactly one package, so normally only one directory matches; unioning is
-- robust to a suite legitimately spanning more than one.)  Each file is a JSON
-- array of 'TestLocation' objects.  A suite whose listing is absent from every
-- directory yields an empty map.
readTestLocations ::
  [Path Abs Dir] ->
  Text ->
  IO (Map.Map TestId (Path Rel File, Word))
readTestLocations coverageDirs suiteName = do
  relFile <- case parseRelFile (T.unpack suiteName ++ ".json") of
    Just rf -> pure rf
    Nothing -> fail ("sydtest-mutation-driver diff: invalid suite name for locations file: " ++ show suiteName)
  let candidates = map (\dir -> dir </> [reldir|test-locations|] </> relFile) coverageDirs
  -- Read directly and treat a missing file as an empty listing via
  -- 'forgivingAbsence', rather than a 'doesFileExist' check that would race
  -- (TOCTOU) against the file disappearing between the check and the read.
  maps <-
    mapM
      ( \path -> do
          mbs <- forgivingAbsence (SB.readFile (fromAbsFile path))
          case mbs of
            Nothing -> pure Map.empty
            Just bs -> case decodeTestLocations bs of
              Just locs -> pure (testLocationsMap locs)
              Nothing -> fail ("sydtest-mutation-driver diff: could not decode test locations from " ++ fromAbsFile path)
      )
      candidates
  pure (Map.unions maps)
  where
    testLocationsMap :: [TestLocation] -> Map.Map TestId (Path Rel File, Word)
    testLocationsMap locs =
      Map.fromList
        [ (testLocationTestId l, (testLocationFile l, testLocationLine l))
        | l <- locs
        ]
