{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Top-level entry point for the @sydtest-mutation-driver@ executable.
--
-- The driver orchestrates the two-phase mutation testing workflow:
--
-- 1. Coverage phase: for each declared suite, spawn the suite executable
--    once per leaf test with @--mutation-coverage-one@ to discover which
--    tests cover which mutations.  Merge the per-suite results into a
--    single @manifest-augmented.json@.
-- 2. Mutation phase: spawn one child per mutation per covering suite;
--    treat exit-zero as "survived" and non-zero as "killed".  Write
--    @report.txt@ and @report.json@ to the configured report directory.
module Test.Syd.Mutation.Driver
  ( sydMutationDriver,
    module Test.Syd.Mutation.Driver.OptParse,
  )
where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Path
import Path.IO (getCurrentDir, setCurrentDir)
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEncoding, stderr, stdout, utf8)
import Test.Syd.Mutation.Driver.Components (runListComponents)
import Test.Syd.Mutation.Driver.Coverage (runCoverageMode)
import Test.Syd.Mutation.Driver.Mutate (runMutationMode)
import Test.Syd.Mutation.Driver.OptParse
import Test.Syd.MutationMode.Common (renderMutationRunReport)
import Text.Colour (TerminalCapabilities (..), renderChunksText, unlinesChunks)

-- | Top-level entry point: parse the dispatch and run the chosen
-- subcommand.  The default subcommand is @run@, which runs both mutation
-- phases (coverage then mutation).
sydMutationDriver :: IO ()
sydMutationDriver = do
  -- Set both streams to UTF-8 so the rendered report (which contains
  -- box-drawing characters and em-dashes) doesn't fail to print when
  -- running in a build sandbox where LANG isn't set.
  hSetEncoding stderr utf8
  hSetEncoding stdout utf8
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering
  dispatch <- getDispatch
  case dispatch of
    DispatchRun settings -> runDriver settings
    DispatchListComponents kind cabalFile -> runListComponents kind cabalFile
    DispatchInstallComponents {} ->
      fail "sydtest-mutation-driver: install-components is not implemented yet"
    DispatchAssertScore _ _ ->
      fail "sydtest-mutation-driver: assert-score is not implemented yet"

-- | Run the driver phases in order: coverage, then mutation.
runDriver :: MutationDriverSettings -> IO ()
runDriver MutationDriverSettings {..} = do
  let suitesAsc = Map.toAscList mutationDriverSettingSuites
  case suitesAsc of
    [] -> fail "sydtest-mutation-driver: no suites configured"
    (_ : _) -> pure ()
  -- Coverage phase: for each suite, cd into its resource directory (if
  -- set) and run the coverage parent against its exe.  The augmented
  -- manifest accumulates across suites: each suite's pass merges into the
  -- prior augmented manifest.
  mapM_
    ( runOneSuiteCoverage
        mutationDriverSettingManifests
        mutationDriverSettingAugmentedManifestDir
        mutationDriverSettingCoverageJobs
        mutationDriverSettingCoverageRetry
        mutationDriverSettingFailFast
    )
    suitesAsc
  -- Mutation phase: cd into the first suite's resource directory (in
  -- Map.toAscList order, which is sorted by key), then run the mutation
  -- parent.  The child suite-exe map is the same map of declared suites.
  let firstSuiteResourceDir = case suitesAsc of
        ((_, sc) : _) -> suiteConfigResourceDir sc
        [] -> Nothing
      suiteExesByName = Map.map suiteConfigExe mutationDriverSettingSuites
  reportText <-
    withMaybeCurrentDir firstSuiteResourceDir $
      runMutationMode
        mutationDriverSettingFailFast
        mutationDriverSettingAugmentedManifestDir
        mutationDriverSettingReportDir
        mutationDriverSettingChildMemLimit
        suiteExesByName
  -- 'runMutationMode' already prints the report to stdout and writes
  -- @report.json@ to the report directory.  Also write @report.txt@ to
  -- the report directory so the Nix harness can install it alongside
  -- @report.json@.
  case mutationDriverSettingReportDir of
    Just reportDir -> do
      let renderedText = renderChunksText With8BitColours (unlinesChunks (renderMutationRunReport reportText))
          reportFile = reportDir </> [relfile|report.txt|]
      -- Write bytes directly so we don't depend on the locale's encoding
      -- being UTF-8 (the report contains em-dashes and box-drawing
      -- characters).
      BS.writeFile (fromAbsFile reportFile) (T.encodeUtf8 renderedText)
    Nothing -> pure ()
  hFlush stdout

-- | Run the coverage phase for one suite, with optional @cd@ into its
-- resource directory beforehand.
runOneSuiteCoverage ::
  [Path Abs Dir] ->
  Maybe (Path Abs Dir) ->
  Maybe Int ->
  Word ->
  Bool ->
  (Text, SuiteConfig) ->
  IO ()
runOneSuiteCoverage manifests augmentedManifestDir coverageJobs coverageRetry failFast (suiteName, SuiteConfig {suiteConfigExe, suiteConfigResourceDir}) =
  withMaybeCurrentDir suiteConfigResourceDir $
    runCoverageMode
      failFast
      manifests
      augmentedManifestDir
      coverageJobs
      coverageRetry
      suiteName
      suiteConfigExe

-- | Bracket-style @cd@ into the given directory for the duration of the
-- action.  Restores the original working directory afterward.
withMaybeCurrentDir :: Maybe (Path Abs Dir) -> IO a -> IO a
withMaybeCurrentDir = \case
  Nothing -> id
  Just dir -> withCurrentDir' dir

-- | Local copy of 'Path.IO.withCurrentDir'-style bracketing.  Restoring
-- the previous directory in 'bracket' guarantees the restore happens
-- even when the action throws.
withCurrentDir' :: Path Abs Dir -> IO a -> IO a
withCurrentDir' dir action =
  bracket getCurrentDir setCurrentDir $ \_ -> do
    setCurrentDir dir
    action
