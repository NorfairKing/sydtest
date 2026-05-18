{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Option, environment, and YAML configuration parsing for the
-- @sydtest-mutation-driver@ executable.
--
-- The driver does not accept many command-line flags by design: nearly
-- everything it needs is passed via a YAML configuration file, the path to
-- which is given via @--config-file@.  A small number of flags are
-- accepted on the command line so that callers (notably the Nix harness)
-- can override path-shaped values that are not known at config-render
-- time, e.g. @--mutation-report-dir@.
module Test.Syd.Mutation.Driver.OptParse
  ( MutationDriverConfig (..),
    SuiteConfig (..),
    MutationDriverSettings (..),
    getMutationDriverSettings,
    resolveMutationDriverSettings,
  )
where

import Autodocodec
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import OptEnvConf
import Path
import Paths_sydtest_mutation_driver (version)

-- | Configuration for one sydtest test suite executable that the driver
-- spawns as a coverage child and as a mutation child.
data SuiteConfig = SuiteConfig
  { -- | Absolute path to the test-suite executable on disk.
    suiteConfigExe :: !(Path Abs File),
    -- | Optional resource directory to @cd@ into before spawning the
    -- executable.  Useful so the test suite can find its golden files and
    -- data files via relative paths, just as it would from a Cabal
    -- @checkPhase@.
    suiteConfigResourceDir :: !(Maybe (Path Abs Dir))
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec SuiteConfig)

instance HasCodec SuiteConfig where
  codec =
    object "SuiteConfig" $
      SuiteConfig
        <$> requiredFieldWith "exe" absFileCodec "Absolute path to the test-suite executable"
          .= suiteConfigExe
        <*> optionalFieldWith "resourceDir" absDirCodec "Directory to cd into before spawning the suite"
          .= suiteConfigResourceDir

-- | Codec for 'Path Abs File' as a JSON string.
absFileCodec :: JSONCodec (Path Abs File)
absFileCodec =
  bimapCodec
    (\s -> maybe (Left ("invalid absolute file path: " ++ T.unpack s)) Right (parseAbsFile (T.unpack s)))
    (T.pack . fromAbsFile)
    codec

-- | Codec for 'Path Abs Dir' as a JSON string.
absDirCodec :: JSONCodec (Path Abs Dir)
absDirCodec =
  bimapCodec
    (\s -> maybe (Left ("invalid absolute directory path: " ++ T.unpack s)) Right (parseAbsDir (T.unpack s)))
    (T.pack . fromAbsDir)
    codec

-- | Top-level configuration for one mutation-driver run.
--
-- Rendered from JSON/YAML; also produced by command-line flags via
-- 'getMutationDriverSettings'.  Path-shaped fields are kept as 'Path'
-- values so the user has to commit to absolute paths.
data MutationDriverConfig = MutationDriverConfig
  { -- | One or more mutation manifest directories.  Each one is the
    -- @manifest@ output of an instrumented Haskell package; together they
    -- enumerate every mutation point under test.
    mutationDriverConfigManifests :: ![Path Abs Dir],
    -- | Test suites to run.  Keyed by suite name.  The same map of names
    -- to exes is passed down to mutation children so they can rediscover
    -- their own suite by name.
    mutationDriverConfigSuites :: !(Map.Map Text SuiteConfig),
    -- | RTS heap cap to apply to each mutation child.  Passed through as
    -- @+RTS -M<limit> -RTS@.  Defaults to no limit.
    mutationDriverConfigChildMemLimit :: !(Maybe String),
    -- | Maximum number of coverage children to run concurrently.  Defaults
    -- to @getNumCapabilities@.
    mutationDriverConfigCoverageJobs :: !(Maybe Int),
    -- | Retry budget for a flaky coverage child.  Defaults to 3.
    mutationDriverConfigCoverageRetry :: !(Maybe Word),
    -- | Directory in which to write @manifest-augmented.json@.  Defaults
    -- to the current working directory of the driver process.
    mutationDriverConfigAugmentedManifestDir :: !(Maybe (Path Abs Dir)),
    -- | Directory in which to write @report.txt@ / @report.json@.  May be
    -- overridden via the @--mutation-report-dir@ flag at run time, because
    -- the Nix harness needs to point this at @$out@ which is not known
    -- when the YAML is rendered.  Defaults to the driver's CWD.
    mutationDriverConfigReportDir :: !(Maybe (Path Abs Dir)),
    -- | Whether to abort the run on the first surviving or uncovered
    -- mutation.  Defaults to 'True'.
    mutationDriverConfigFailFast :: !(Maybe Bool)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec MutationDriverConfig)

instance HasCodec MutationDriverConfig where
  codec =
    object "MutationDriverConfig" $
      MutationDriverConfig
        <$> optionalFieldWithDefaultWith "manifests" (listCodec absDirCodec) [] "Paths to manifest directories"
          .= mutationDriverConfigManifests
        <*> optionalFieldWithDefault "suites" Map.empty "Test suites to run, keyed by suite name"
          .= mutationDriverConfigSuites
        <*> optionalField "childMemLimit" "RTS heap cap for each mutation child, e.g. 4g"
          .= mutationDriverConfigChildMemLimit
        <*> optionalField "coverageJobs" "Maximum number of coverage children to run concurrently"
          .= mutationDriverConfigCoverageJobs
        <*> optionalField "coverageRetry" "How many times to retry a failing coverage child"
          .= mutationDriverConfigCoverageRetry
        <*> optionalFieldWith "augmentedManifestDir" absDirCodec "Directory for manifest-augmented.json"
          .= mutationDriverConfigAugmentedManifestDir
        <*> optionalFieldWith "reportDir" absDirCodec "Directory in which to write report.txt and report.json"
          .= mutationDriverConfigReportDir
        <*> optionalField "failFast" "Whether to abort on the first surviving or uncovered mutation"
          .= mutationDriverConfigFailFast

-- | The fully resolved settings for one driver run.  This is what
-- 'getMutationDriverSettings' returns; it merges the YAML config with any
-- command-line overrides.
data MutationDriverSettings = MutationDriverSettings
  { mutationDriverSettingManifests :: ![Path Abs Dir],
    mutationDriverSettingSuites :: !(Map.Map Text SuiteConfig),
    mutationDriverSettingChildMemLimit :: !(Maybe String),
    mutationDriverSettingCoverageJobs :: !(Maybe Int),
    mutationDriverSettingCoverageRetry :: !Word,
    mutationDriverSettingAugmentedManifestDir :: !(Maybe (Path Abs Dir)),
    mutationDriverSettingReportDir :: !(Maybe (Path Abs Dir)),
    mutationDriverSettingFailFast :: !Bool
  }
  deriving (Show, Eq, Generic)

-- | The opt-env-conf parser for the driver.
--
-- The CLI exposes:
--   * @--config-file PATH@ (added implicitly by 'withConfigurableYamlConfig').
--   * @--mutation-report-dir DIR@: overrides the YAML 'reportDir'.
--
-- All other configuration is read from the YAML config file via 'conf'.
mutationDriverConfigParser :: Parser MutationDriverConfig
mutationDriverConfigParser = do
  mutationDriverConfigManifests <-
    setting
      [ help "Mutation manifest directories",
        confWith "manifests" (listCodec absDirCodec),
        value []
      ]
  mutationDriverConfigSuites <-
    setting
      [ help "Test suites to run, keyed by suite name",
        conf "suites",
        value Map.empty
      ]
  mutationDriverConfigChildMemLimit <-
    optional $
      setting
        [ help "RTS heap cap for each mutation child, e.g. 4g",
          conf "childMemLimit"
        ]
  mutationDriverConfigCoverageJobs <-
    optional $
      setting
        [ help "Maximum number of coverage children to run concurrently",
          conf "coverageJobs"
        ]
  mutationDriverConfigCoverageRetry <-
    optional $
      setting
        [ help "How many times to retry a failing coverage child",
          conf "coverageRetry"
        ]
  mutationDriverConfigAugmentedManifestDir <-
    optional $
      choice
        [ directoryPathSetting
            [ help "Directory for manifest-augmented.json (overrides the config file)",
              option,
              long "mutation-augmented-manifest-dir"
            ],
          setting
            [ help "Directory for manifest-augmented.json",
              confWith "augmentedManifestDir" absDirCodec
            ]
        ]
  mutationDriverConfigReportDir <-
    optional $
      choice
        [ directoryPathSetting
            [ help "Directory for report.txt and report.json (overrides the config file)",
              option,
              long "mutation-report-dir"
            ],
          setting
            [ help "Directory for report.txt and report.json",
              confWith "reportDir" absDirCodec
            ]
        ]
  mutationDriverConfigFailFast <-
    optional $
      setting
        [ help "Whether to abort on the first surviving or uncovered mutation",
          conf "failFast"
        ]
  pure MutationDriverConfig {..}

instance HasParser MutationDriverConfig where
  settingsParser =
    subEnv_ "sydtest_mutation_driver" $
      withLocalYamlConfig mutationDriverConfigParser

-- | Resolve a 'MutationDriverConfig' into the run settings, filling in
-- defaults for fields the user did not specify.
resolveMutationDriverSettings :: MutationDriverConfig -> MutationDriverSettings
resolveMutationDriverSettings MutationDriverConfig {..} =
  MutationDriverSettings
    { mutationDriverSettingManifests = mutationDriverConfigManifests,
      mutationDriverSettingSuites = mutationDriverConfigSuites,
      mutationDriverSettingChildMemLimit = mutationDriverConfigChildMemLimit,
      mutationDriverSettingCoverageJobs = mutationDriverConfigCoverageJobs,
      mutationDriverSettingCoverageRetry = fromMaybe defaultCoverageRetry mutationDriverConfigCoverageRetry,
      mutationDriverSettingAugmentedManifestDir = mutationDriverConfigAugmentedManifestDir,
      mutationDriverSettingReportDir = mutationDriverConfigReportDir,
      mutationDriverSettingFailFast = fromMaybe defaultFailFast mutationDriverConfigFailFast
    }

-- | Default coverage retry budget.  Set conservatively so a flaky coverage
-- child does not lose the entire run.
defaultCoverageRetry :: Word
defaultCoverageRetry = 3

-- | Default fail-fast.  True suits CI so a single survivor aborts the run;
-- flip to False locally for the full report.
defaultFailFast :: Bool
defaultFailFast = True

-- | Parse the driver's settings from argv, env, and YAML.
getMutationDriverSettings :: IO MutationDriverSettings
getMutationDriverSettings = do
  config <- runSettingsParser version "Out-of-process mutation testing driver for sydtest"
  pure (resolveMutationDriverSettings config)
