{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.IO as TIO
import Path
import Path.IO (doesFileExist, withSystemTempDir)
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (proc, readProcessStdout_)
import Test.Syd.Mutation.AugmentedManifest
  ( filterAugmentedManifestByIds,
    readAugmentedManifestFile,
    writeAugmentedManifestFile,
  )
import Test.Syd.Mutation.Driver.Diff (parseTestLocationsTsv, parseUnifiedDiff, selectMutations)
import Test.Syd.Mutation.Driver.Mutate (runMutationMode)
import Test.Syd.Mutation.Driver.OptParse
  ( DiffSettings (..),
    DiffSource (..),
    SuiteConfig (..),
  )
import Test.Syd.Mutation.Driver.SuitePkg (walkSuitePkgs)
import Test.Syd.Mutation.TestId (TestId)

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

  -- 3. Read the cached augmented manifest and the cached per-suite test
  -- location listings.
  manifest <- readAugmentedManifestFile diffSettingAugmentedManifestDir
  testLocationsBySuite <-
    Map.traverseWithKey
      (\suiteName _ -> readTestLocations diffSettingTestLocationsDir suiteName)
      suites

  -- 4. Select the diff-implied mutations and filter the manifest down to them.
  let selected = selectMutations hunks manifest testLocationsBySuite
      filtered = filterAugmentedManifestByIds selected manifest

  hPutStrLn stderr $
    "sydtest-mutation-driver diff: "
      ++ show (length hunks)
      ++ " changed hunk(s); selected "
      ++ show (length selected)
      ++ " mutation(s) to run."

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
-- @<dir>/<suite-name>.tsv@.  Each line is @<rendered-test-id>\\t<file>:<line>@;
-- a line without a tab (a test whose call stack was empty) is skipped, as it
-- cannot be mapped to a source line.  A missing file yields an empty map (the
-- suite contributed no listing).
readTestLocations ::
  Path Abs Dir ->
  Text ->
  IO (Map.Map TestId (Path Rel File, Word))
readTestLocations dir suiteName = do
  relFile <- case parseRelFile (T.unpack suiteName ++ ".tsv") of
    Just rf -> pure rf
    Nothing -> fail ("sydtest-mutation-driver diff: invalid suite name for tsv file: " ++ show suiteName)
  let path = dir </> relFile
  exists <- doesFileExist path
  if not exists
    then pure Map.empty
    else parseTestLocationsTsv <$> TIO.readFile (fromAbsFile path)
