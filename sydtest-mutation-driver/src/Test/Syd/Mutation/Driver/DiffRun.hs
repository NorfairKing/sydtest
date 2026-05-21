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
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.IO as TIO
import Path
import Path.IO (forgivingAbsence, withSystemTempDir)
import System.IO (stderr)
import System.Process.Typed (proc, readProcessStdout_)
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    filterAugmentedManifestByIds,
    mergeAugmentedManifests,
    readAugmentedManifestFile,
    writeAugmentedManifestFile,
  )
import Test.Syd.Mutation.Driver.Diff (parseUnifiedDiff, selectMutations)
import Test.Syd.Mutation.Driver.Mutate (runMutationMode)
import Test.Syd.Mutation.Driver.OptParse
  ( DiffSettings (..),
    DiffSource (..),
    SuiteConfig (..),
  )
import Test.Syd.Mutation.Driver.SuitePkg (walkSuitePkgs)
import Test.Syd.Mutation.TestId (TestId)
import Test.Syd.Mutation.TestLocation (TestLocation (..), decodeTestLocations)
import Text.Colour (Chunk, TerminalCapabilities (..), chunk, cyan, fore, green, hPutChunksLocaleWith, unlinesChunks)

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
  let selected = selectMutations hunks manifest testLocationsBySuite
      filtered = filterAugmentedManifestByIds selected manifest

  hPutChunksLocaleWith With8BitColours stderr $
    unlinesChunks (renderDiffSelection (length hunks) (length selected))

  -- 5. Run the mutation phase over the filtered manifest.  'runMutationMode'
  -- reads the augmented manifest from a directory, so write the filtered
  -- manifest to a temp dir and point it there.
  withSystemTempDir "mutation-diff-augmented" $ \augDir -> do
    writeAugmentedManifestFile augDir filtered
    let suiteExes = Map.map suiteConfigExe suites
    _ <-
      runMutationMode
        diffSettingFailFast
        augDir
        diffSettingOutDir
        diffSettingChildMemLimit
        suiteExes
    pure ()

-- | Render the one-line "N changed hunks; selected M mutations" progress
-- message as coloured chunks, matching the rest of the driver's output.
renderDiffSelection :: Int -> Int -> [[Chunk]]
renderDiffSelection numHunks numSelected =
  [ [ chunk "diff: ",
      fore cyan (chunk (T.pack (show numHunks))),
      chunk (plural numHunks " changed hunk" " changed hunks"),
      chunk "; selected ",
      fore green (chunk (T.pack (show numSelected))),
      chunk (plural numSelected " mutation to run." " mutations to run.")
    ]
  ]
  where
    plural n one many = if n == 1 then one else many

-- | Obtain the unified diff text from the configured source.
obtainDiff :: DiffSource -> IO Text
obtainDiff = \case
  DiffSourceFile f -> TIO.readFile (fromAbsFile f)
  DiffSourceStdin -> TIO.getContents
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
          mbs <- forgivingAbsence (B.readFile (fromAbsFile path))
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
