{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Command-line option parsing for the @sydtest-mutation-driver@
-- executable.  Everything the driver needs comes from CLI flags — no
-- YAML config file, no env vars.
module Test.Syd.Mutation.Driver.OptParse
  ( -- * Top-level dispatch
    Dispatch (..),
    ComponentKind (..),
    getDispatch,

    -- * 'run' subcommand
    SuiteConfig (..),
    SuitePkgSpec (..),
    parseSuitePkgSpec,
    MutationDriverSettings (..),
    defaultCoverageRetry,
    defaultFailFast,

    -- * 'coverage' subcommand
    CoverageSettings (..),

    -- * 'diff' subcommand
    DiffSettings (..),
    DiffSource (..),
    defaultBaseBranch,

    -- * 'redundancy' subcommand
    RedundancySettings (..),
  )
where

import GHC.Generics (Generic)
import OptEnvConf
import Path
import Paths_sydtest_mutation_driver (version)
import Test.Syd.Mutation.Redundancy (RedundancyBasis (..))

-- | Configuration for one sydtest test suite executable that the driver
-- spawns as a coverage child and as a mutation child.  The driver builds
-- the @Map Text SuiteConfig@ at run time by walking each
-- 'SuitePkgSpec'.
data SuiteConfig = SuiteConfig
  { -- | Absolute path to the test-suite executable on disk.
    suiteConfigExe :: !(Path Abs File),
    -- | Optional resource directory to @cd@ into before spawning the
    -- executable.  Useful so the test suite can find its golden files and
    -- data files via relative paths, just as it would from a Cabal
    -- @checkPhase@.
    suiteConfigResourceDir :: !(Maybe (Path Abs Dir))
  }
  deriving (Show, Eq, Generic)

-- | Specification for one test-package whose installed test-suite
-- executables should be added to the run's suite map.  Parsed from a
-- @--suite-pkg PNAME=BUILT_TEST_PKG_ROOT=RESOURCE_DIR@ flag.
--
-- At run time the driver walks @<built-test-pkg-root>/test/*@ and
-- emits one 'SuiteConfig' per file, keyed by the file's basename.  The
-- resource dir is recorded on every emitted suite.
--
-- The package name does not directly appear in the suite map (suites
-- are keyed by their executable basename, matching the previous bash
-- behaviour) but is carried for error-message attribution.
data SuitePkgSpec = SuitePkgSpec
  { suitePkgSpecPname :: !String,
    suitePkgSpecBuiltTestPkgRoot :: !(Path Abs Dir),
    suitePkgSpecResourceDir :: !(Path Abs Dir)
  }
  deriving (Show, Eq, Generic)

-- | Parse a @PNAME=BUILT_TEST_PKG_ROOT=RESOURCE_DIR@ spec.  '=' is used
-- as the delimiter because Cabal package names exclude it and Nix store
-- paths never contain it; absolute paths in normal use never contain it
-- either.
parseSuitePkgSpec :: String -> Either String SuitePkgSpec
parseSuitePkgSpec s = case splitOnEq s of
  [pname, root, rd]
    | null pname -> Left ("empty pname in --suite-pkg spec: " ++ show s)
    | otherwise -> do
        rootDir <-
          maybe
            (Left ("invalid built-test-pkg root in --suite-pkg spec: " ++ root))
            Right
            (parseAbsDir root)
        rdDir <-
          maybe
            (Left ("invalid resource dir in --suite-pkg spec: " ++ rd))
            Right
            (parseAbsDir rd)
        Right
          SuitePkgSpec
            { suitePkgSpecPname = pname,
              suitePkgSpecBuiltTestPkgRoot = rootDir,
              suitePkgSpecResourceDir = rdDir
            }
  _ -> Left ("--suite-pkg expects PNAME=BUILT_TEST_PKG_ROOT=RESOURCE_DIR, got: " ++ show s)
  where
    splitOnEq :: String -> [String]
    splitOnEq input = case break (== '=') input of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitOnEq rest

-- | The fully resolved settings for one driver run.  Produced directly
-- from CLI flags; the @--suite-pkg@ specs are expanded into the suite
-- map at run time, not at parse time, so the parser does not touch the
-- filesystem.
data MutationDriverSettings = MutationDriverSettings
  { mutationDriverSettingManifests :: ![Path Abs Dir],
    mutationDriverSettingSuitePkgs :: ![SuitePkgSpec],
    mutationDriverSettingChildMemLimit :: !(Maybe String),
    mutationDriverSettingCoverageJobs :: !(Maybe Word),
    mutationDriverSettingCoverageRetry :: !Word,
    mutationDriverSettingAugmentedManifestDir :: !(Path Abs Dir),
    mutationDriverSettingOutDir :: !(Path Abs Dir),
    mutationDriverSettingFailFast :: !Bool
  }
  deriving (Show, Eq, Generic)

-- | Settings for the @coverage@ subcommand: build only the coverage cache
-- (the augmented manifest) that the diff-scoped runner depends on, without
-- running the mutation phase.  This is what lets the diff cache be a much
-- cheaper derivation than the full check.
data CoverageSettings = CoverageSettings
  { coverageSettingManifests :: ![Path Abs Dir],
    coverageSettingSuitePkgs :: ![SuitePkgSpec],
    coverageSettingCoverageJobs :: !(Maybe Word),
    coverageSettingCoverageRetry :: !Word,
    coverageSettingAugmentedManifestDir :: !(Path Abs Dir),
    coverageSettingFailFast :: !Bool
  }
  deriving (Show, Eq, Generic)

-- | Where the @diff@ subcommand gets the unified diff to scope the run by.
data DiffSource
  = -- | Read the diff from this file.
    DiffSourceFile !(Path Abs File)
  | -- | Read the diff from standard input.
    DiffSourceStdin
  | -- | Compute @git diff@ against the merge-base of @HEAD@ and the given
    -- base branch, run from this working directory.  This is the default.
    DiffSourceGitMergeBase
      -- | Base branch to find the merge-base with.
      !String
  deriving (Show, Eq, Generic)

-- | Settings for the @diff@ subcommand: the diff-scoped mutation runner.
--
-- It reads the /already-built/ augmented manifest (the cached
-- which-test-covers-which-mutation map) and the cached per-suite
-- @TestId -> source-location@ listings, selects the mutations implied by the
-- diff, and runs only those mutation children.  No compilation and no
-- coverage phase happen.
data DiffSettings = DiffSettings
  { -- | Where the unified diff comes from.
    diffSettingSource :: !DiffSource,
    -- | Test-package specs, expanded to the suite-exe map at run time
    -- (same as the @run@ subcommand).
    diffSettingSuitePkgs :: ![SuitePkgSpec],
    -- | Per-package coverage directories.  Each directory holds that
    -- package's @augmented/manifest-augmented.json@ and its
    -- @test-locations/\<suite\>.tsv@ listings.  The driver unions the
    -- augmented manifests in-memory and reads every directory's
    -- test-locations, so no separate merge step is needed.
    diffSettingCoverageDirs :: ![Path Abs Dir],
    -- | RTS heap cap for each mutation child.
    diffSettingChildMemLimit :: !(Maybe String),
    -- | Output directory for report.json\/report.txt\/per-suite logs.
    diffSettingOutDir :: !(Path Abs Dir),
    -- | Whether to abort on the first surviving or uncovered mutation.
    diffSettingFailFast :: !Bool
  }
  deriving (Show, Eq, Generic)

-- | Settings for the @redundancy@ subcommand: the redundant-test analysis.
--
-- The @coverage@ basis reads only the cached augmented manifest(s) from
-- @--coverage-dir@ and needs no test runs (no @--suite-pkg@,
-- @--child-mem-limit@).  The @kill@ basis additionally runs each mutation's
-- covering tests to observe per-test kills, so it needs the suite exes.
data RedundancySettings = RedundancySettings
  { -- | Which relation to analyse.
    redundancySettingBasis :: !RedundancyBasis,
    -- | Test-package specs, expanded to the suite-exe map at run time.  Only
    -- used by the @kill@ basis.
    redundancySettingSuitePkgs :: ![SuitePkgSpec],
    -- | Per-package coverage directories, each holding
    -- @augmented/manifest-augmented.json@ (same layout as @diff@).
    redundancySettingCoverageDirs :: ![Path Abs Dir],
    -- | RTS heap cap for each kill-matrix child.  Only used by the @kill@
    -- basis.
    redundancySettingChildMemLimit :: !(Maybe String),
    -- | Output directory for redundancy.json\/redundancy.txt.
    redundancySettingOutDir :: !(Path Abs Dir)
  }
  deriving (Show, Eq, Generic)

-- | Default base branch the @diff@ subcommand finds the merge-base with when
-- no @--diff@ is given and no @--base@ is set.
defaultBaseBranch :: String
defaultBaseBranch = "master"

-- | Top-level CLI parser for the @run@ subcommand.
mutationDriverSettingsParser :: Parser MutationDriverSettings
mutationDriverSettingsParser = do
  mutationDriverSettingManifests <-
    many $
      directoryPathSetting
        [ help "Mutation manifest directory (one per instrumented library; may be repeated)",
          option,
          long "manifest",
          metavar "DIR"
        ]
  mutationDriverSettingSuitePkgs <-
    many $
      setting
        [ help "Test-package: PNAME=BUILT_TEST_PKG_ROOT=RESOURCE_DIR (may be repeated)",
          reader $ eitherReader parseSuitePkgSpec,
          option,
          long "suite-pkg",
          metavar "PNAME=ROOT=RESOURCE_DIR"
        ]
  mutationDriverSettingChildMemLimit <-
    optional $
      setting
        [ help "RTS heap cap for each mutation child, e.g. 4g",
          reader str,
          option,
          long "child-mem-limit",
          metavar "LIMIT"
        ]
  mutationDriverSettingCoverageJobs <-
    optional $
      setting
        [ help "Maximum number of coverage children to run concurrently",
          reader auto,
          option,
          long "coverage-jobs",
          metavar "INT"
        ]
  mutationDriverSettingCoverageRetry <-
    setting
      [ help "How many times to retry a failing coverage child before giving up",
        reader auto,
        option,
        long "coverage-retry",
        metavar "INT",
        value defaultCoverageRetry
      ]
  mutationDriverSettingAugmentedManifestDir <-
    directoryPathSetting
      [ help "Directory for manifest-augmented.json (required)",
        option,
        long "mutation-augmented-manifest-dir"
      ]
  mutationDriverSettingOutDir <-
    directoryPathSetting
      [ help "Output directory: where the driver writes report.txt, report.json, and per-suite *.log files (required)",
        option,
        long "out-dir"
      ]
  mutationDriverSettingFailFast <-
    yesNoSwitch
      [ help "Whether to abort on the first surviving or uncovered mutation",
        long "fail-fast",
        value defaultFailFast
      ]
  pure MutationDriverSettings {..}

-- | CLI parser for the @coverage@ subcommand.  Shares the coverage-relevant
-- flags with @run@ but takes no @--out-dir@ (it writes no report) and no
-- @--child-mem-limit@ (it spawns no mutation children).
coverageSettingsParser :: Parser CoverageSettings
coverageSettingsParser = do
  coverageSettingManifests <-
    many $
      directoryPathSetting
        [ help "Mutation manifest directory (one per instrumented library; may be repeated)",
          option,
          long "manifest",
          metavar "DIR"
        ]
  coverageSettingSuitePkgs <-
    many $
      setting
        [ help "Test-package: PNAME=BUILT_TEST_PKG_ROOT=RESOURCE_DIR (may be repeated)",
          reader $ eitherReader parseSuitePkgSpec,
          option,
          long "suite-pkg",
          metavar "PNAME=ROOT=RESOURCE_DIR"
        ]
  coverageSettingCoverageJobs <-
    optional $
      setting
        [ help "Maximum number of coverage children to run concurrently",
          reader auto,
          option,
          long "coverage-jobs",
          metavar "INT"
        ]
  coverageSettingCoverageRetry <-
    setting
      [ help "How many times to retry a failing coverage child before giving up",
        reader auto,
        option,
        long "coverage-retry",
        metavar "INT",
        value defaultCoverageRetry
      ]
  coverageSettingAugmentedManifestDir <-
    directoryPathSetting
      [ help "Directory to write manifest-augmented.json into (required)",
        option,
        long "mutation-augmented-manifest-dir"
      ]
  coverageSettingFailFast <-
    yesNoSwitch
      [ help "Whether to abort the coverage run on a baseline test failure",
        long "fail-fast",
        value defaultFailFast
      ]
  pure CoverageSettings {..}

-- | CLI parser for the @diff@ subcommand.
diffSettingsParser :: Parser DiffSettings
diffSettingsParser = do
  diffSettingSource <- diffSourceParser
  diffSettingSuitePkgs <-
    many $
      setting
        [ help "Test-package: PNAME=BUILT_TEST_PKG_ROOT=RESOURCE_DIR (may be repeated)",
          reader $ eitherReader parseSuitePkgSpec,
          option,
          long "suite-pkg",
          metavar "PNAME=ROOT=RESOURCE_DIR"
        ]
  diffSettingCoverageDirs <-
    many $
      directoryPathSetting
        [ help "Per-package coverage directory holding augmented/ and test-locations/ (may be repeated)",
          option,
          long "coverage-dir",
          metavar "DIR"
        ]
  diffSettingChildMemLimit <-
    optional $
      setting
        [ help "RTS heap cap for each mutation child, e.g. 4g",
          reader str,
          option,
          long "child-mem-limit",
          metavar "LIMIT"
        ]
  diffSettingOutDir <-
    directoryPathSetting
      [ help "Output directory: where the driver writes report.txt, report.json, and per-suite *.log files (required)",
        option,
        long "out-dir"
      ]
  diffSettingFailFast <-
    yesNoSwitch
      [ help "Whether to abort on the first surviving or uncovered mutation",
        long "fail-fast",
        value defaultFailFast
      ]
  pure DiffSettings {..}

-- | Parse the diff source.  Precedence: an explicit @--diff FILE@ wins, then
-- @--diff-stdin@, otherwise the default git merge-base computation against
-- @--base@ (defaulting to 'defaultBaseBranch').
diffSourceParser :: Parser DiffSource
diffSourceParser = do
  mFile <-
    optional $
      filePathSetting
        [ help "Read the unified diff from this file instead of computing it from git",
          option,
          long "diff",
          metavar "FILE"
        ]
  fromStdin <-
    setting
      [ help "Read the unified diff from standard input instead of computing it from git",
        switch True,
        long "diff-stdin",
        value False
      ]
  base <-
    setting
      [ help "Base branch to compute the merge-base diff against (default git mode)",
        reader str,
        option,
        long "base",
        metavar "BRANCH",
        value defaultBaseBranch
      ]
  pure $ case mFile of
    Just f -> DiffSourceFile f
    Nothing
      | fromStdin -> DiffSourceStdin
      | otherwise -> DiffSourceGitMergeBase base

-- | CLI parser for the @redundancy@ subcommand.
redundancySettingsParser :: Parser RedundancySettings
redundancySettingsParser = do
  redundancySettingBasis <-
    setting
      [ help "Which relation to analyse: 'coverage' (cheap, approximate — mutations reached) or 'kill' (accurate — mutations caught)",
        reader $ eitherReader $ \case
          "coverage" -> Right BasisCoverage
          "kill" -> Right BasisKill
          s -> Left ("expected 'coverage' or 'kill', got: " ++ s),
        option,
        long "basis",
        metavar "BASIS",
        value BasisCoverage
      ]
  redundancySettingSuitePkgs <-
    many $
      setting
        [ help "Test-package: PNAME=BUILT_TEST_PKG_ROOT=RESOURCE_DIR (may be repeated; only used by the kill basis)",
          reader $ eitherReader parseSuitePkgSpec,
          option,
          long "suite-pkg",
          metavar "PNAME=ROOT=RESOURCE_DIR"
        ]
  redundancySettingCoverageDirs <-
    many $
      directoryPathSetting
        [ help "Per-package coverage directory holding augmented/ (may be repeated)",
          option,
          long "coverage-dir",
          metavar "DIR"
        ]
  redundancySettingChildMemLimit <-
    optional $
      setting
        [ help "RTS heap cap for each kill-matrix child, e.g. 4g",
          reader str,
          option,
          long "child-mem-limit",
          metavar "LIMIT"
        ]
  redundancySettingOutDir <-
    directoryPathSetting
      [ help "Output directory: where the driver writes redundancy.txt and redundancy.json (required)",
        option,
        long "out-dir"
      ]
  pure RedundancySettings {..}

-- | Which component kind to enumerate or install: Cabal @executables@ or
-- @test-suites@.
data ComponentKind = ComponentExecutables | ComponentTestSuites
  deriving (Show, Eq, Generic)

componentKindSetting :: Parser ComponentKind
componentKindSetting =
  setting
    [ help "Which component kind to operate on",
      reader $ eitherReader $ \case
        "executables" -> Right ComponentExecutables
        "test-suites" -> Right ComponentTestSuites
        s -> Left ("expected 'executables' or 'test-suites', got: " ++ s),
      argument,
      metavar "KIND"
    ]

absDirArgument :: String -> Parser (Path Abs Dir)
absDirArgument helpText =
  setting
    [ help helpText,
      reader $ eitherReader $ \s ->
        maybe (Left ("invalid absolute directory path: " ++ s)) Right (parseAbsDir s),
      argument,
      metavar "DIR"
    ]

pnameArgument :: Parser String
pnameArgument =
  setting
    [ help "Cabal package name; the driver resolves <pname>.cabal in the current working directory",
      reader str,
      argument,
      metavar "PNAME"
    ]

-- | What the driver should do this invocation.  Each constructor corresponds
-- to one subcommand.  The default (no subcommand) is 'DispatchRun'.
data Dispatch
  = -- | Run the coverage and mutation phases (the original driver flow).
    DispatchRun !MutationDriverSettings
  | -- | Print the component names of one kind in a cabal file, one per
    -- line.  Used by the Nix harness to enumerate executables and
    -- test-suites at build time.  The cabal file is looked up by
    -- 'Test.Syd.Mutation.Driver.Components.findCabalFile' relative to
    -- the driver's current working directory.
    DispatchListComponents !ComponentKind !String
  | -- | Install (copy) the built executables of one kind from
    -- @dist/build/<n>/<n>@ into the given output directory.  Used by the
    -- Nix harness in @postInstall@.  The cabal file is looked up by
    -- 'Test.Syd.Mutation.Driver.Components.findCabalFile' relative to
    -- the driver's current working directory.
    DispatchInstallComponents !ComponentKind !String !(Path Abs Dir)
  | -- | Check a @report.json@ for survivors and (optionally) uncovered
    -- mutations.  Exits non-zero on assertion failure.  On success,
    -- when 'Just' an output directory is given, symlink @report.txt@
    -- and @report.json@ from the report directory into the output
    -- directory.  This second job exists so the Nix harness's
    -- @assertMutationScore@ can be a single subcommand invocation
    -- rather than a Bash buildCommand with shell-level
    -- @mkdir -p $out; ln -s ...@.
    DispatchAssertScore
      -- | Whether to also fail on uncovered mutations.
      !Bool
      -- | Report directory containing @report.json@ and @report.txt@.
      !(Path Abs Dir)
      -- | Optional output directory to symlink the report files into.
      !(Maybe (Path Abs Dir))
  | -- | Run only the coverage phase, writing the augmented manifest.  This
    -- builds the cheap coverage cache the diff-scoped runner depends on,
    -- without the full mutation run.
    DispatchCoverage !CoverageSettings
  | -- | Run the mutation phase over only the subset of mutations implied by
    -- a diff, using the cached augmented manifest and cached per-suite test
    -- location listings.  No coverage phase; no compilation.
    DispatchDiff !DiffSettings
  | -- | Analyse which tests are redundant with respect to mutation testing,
    -- under the coverage or kill basis.
    DispatchRedundancy !RedundancySettings
  deriving (Show, Eq, Generic)

dispatchParser :: Parser Dispatch
dispatchParser =
  commands
    [ command "run" "Run the coverage and mutation phases" $
        withoutConfig $
          DispatchRun <$> mutationDriverSettingsParser,
      command "list-components" "Print the component names of one kind in a cabal file" $
        withoutConfig $
          DispatchListComponents
            <$> componentKindSetting
            <*> pnameArgument,
      command "install-components" "Copy built executables to an install directory" $
        withoutConfig $
          DispatchInstallComponents
            <$> componentKindSetting
            <*> pnameArgument
            <*> absDirArgument "Output directory to copy executables into",
      command "assert-score" "Check a mutation-run report for survivors and (optionally) uncovered mutations" $
        withoutConfig $
          DispatchAssertScore
            <$> yesNoSwitch
              [ help "Also fail on uncovered mutations",
                long "assert-none-uncovered",
                value True
              ]
            <*> absDirArgument "Directory containing report.json and report.txt"
            <*> optional
              ( setting
                  [ help "Directory to symlink report.txt and report.json into on success",
                    reader $ eitherReader $ \s ->
                      maybe (Left ("invalid absolute directory path: " ++ s)) Right (parseAbsDir s),
                    option,
                    long "out-dir",
                    metavar "DIR"
                  ]
              ),
      command "coverage" "Run only the coverage phase and write the augmented manifest" $
        withoutConfig $
          DispatchCoverage <$> coverageSettingsParser,
      command "diff" "Run only the mutations implied by a diff, against cached coverage" $
        withoutConfig $
          DispatchDiff <$> diffSettingsParser,
      command "redundancy" "Analyse which tests are redundant with respect to mutation testing" $
        withoutConfig $
          DispatchRedundancy <$> redundancySettingsParser,
      defaultCommand "run"
    ]

-- | Default coverage retry budget.  Set conservatively so a flaky coverage
-- child does not lose the entire run.
defaultCoverageRetry :: Word
defaultCoverageRetry = 3

-- | Default fail-fast.  True suits CI so a single survivor aborts the run;
-- flip to False locally for the full report.
defaultFailFast :: Bool
defaultFailFast = True

-- | Parse the top-level dispatch from argv only.  The driver no longer
-- reads environment variables or YAML config files; everything goes
-- through CLI flags.
getDispatch :: IO Dispatch
getDispatch =
  runParser
    version
    "Out-of-process mutation testing driver for sydtest"
    dispatchParser
