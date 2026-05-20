{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.Mutation.Plugin.OptParse
  ( Settings (..),
    defaultSettings,
    parseSettings,
    resolveSettings,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import GHC.Generics (Generic)
import OptEnvConf
import qualified OptEnvConf.Args as Args
import qualified OptEnvConf.EnvMap as EnvMap
import OptEnvConf.Error (ParseError, renderErrors)
import Path
import System.Environment (getEnvironment)
import qualified Text.Colour as Colour

-- | Plugin settings.  One coherent record covering every input source:
--
-- * @--<name>=VALUE@ flags supplied by GHC's plugin-option mechanism,
-- * @MUTATION_PLUGIN_<NAME>@ environment variables,
-- * keys in the YAML file referenced by @--config=PATH@.
--
-- See 'settingsParser' for the source breakdown of each field.
data Settings = Settings
  { -- | Manifest output directory (one JSON file per module written here).
    -- Optional — when absent, the plugin instruments but does not write a
    -- manifest (useful in coverage-only runs).
    settingManifestDir :: !(Maybe (Path Abs Dir)),
    -- | Runtime kill switch: when true, the plugin loads but instruments
    -- nothing.  Set by the Nix two-stage build for the post-library build
    -- phase (see @nix/addManifest.nix@).
    settingSkipInstrumentation :: !Bool,
    -- | Module names to skip during instrumentation.
    settingExceptions :: ![String],
    -- | Mutation operator names disabled globally.
    settingDisabledMutations :: ![String],
    -- | Unqualified identifier names whose calls (and argument subtrees)
    -- the plugin leaves untouched.  See 'Test.Syd.Mutation.Plugin.Instrument'
    -- for how this is consumed.
    settingIgnore :: ![String],
    -- | Skip mutations inside TH splices and quasi-quotes.
    settingSkipThSplices :: !Bool,
    -- | Print each mutation site as it is recorded.
    settingDebug :: !Bool
  }
  deriving (Show, Eq, Generic)

-- | The settings the plugin uses when no @--config=PATH@ is passed and
-- no environment variables are set.  Equivalent to running the parser
-- with empty args, empty env, and no config object.
defaultSettings :: Settings
defaultSettings =
  Settings
    { settingManifestDir = Nothing,
      settingSkipInstrumentation = False,
      settingExceptions = [],
      settingDisabledMutations = [],
      settingIgnore = [],
      settingSkipThSplices = True,
      settingDebug = False
    }

-- | Resolve plugin settings from GHC's plugin opts and the process
-- environment.  Reads any YAML referenced by @--config=PATH@.  Errors
-- out with a rendered opt-env-conf error message on parse failure —
-- better than silently using defaults.
resolveSettings :: [String] -> IO Settings
resolveSettings opts = do
  envPairs <- getEnvironment
  let args = Args.parseArgs opts
      envMap = EnvMap.parse envPairs
  result <- runParserOn allCapabilities Nothing (settingsParser :: Parser Settings) args envMap Nothing
  case result of
    Right s -> pure s
    Left errs ->
      ioError $
        userError $
          "mutation: failed to parse plugin settings:\n"
            ++ renderParseErrors errs

renderParseErrors :: NonEmpty ParseError -> String
renderParseErrors =
  T.unpack . Colour.renderChunksText Colour.WithoutColours . renderErrors

instance HasParser Settings where
  settingsParser = withLocalYamlConfig parseSettings

-- | Field-by-field parser.  Does not include 'withYamlConfig', so callers
-- (and tests) can supply the conf object directly.
parseSettings :: Parser Settings
parseSettings =
  subEnv_ "mutation_plugin" $ do
    settingManifestDir <-
      optional $
        directoryPathSetting
          [ help "Manifest output directory (one JSON file per instrumented module)",
            option,
            long "manifest",
            env "MANIFEST_DIR",
            metavar "DIR"
          ]
    settingSkipInstrumentation <-
      setting
        [ help "When set, load the plugin but instrument nothing.  Used by the Nix two-stage build.",
          switch True,
          long "skip",
          reader exists,
          env "SKIP",
          metavar "ANY",
          value False
        ]
    settingExceptions <-
      setting
        [ help "Module names to skip during instrumentation",
          reader $ eitherReader (Right . splitComma),
          option,
          long "exceptions",
          env "EXCEPTIONS",
          conf "exceptions",
          metavar "NAME,NAME,...",
          value []
        ]
    settingDisabledMutations <-
      setting
        [ help "Mutation operator names to disable globally",
          reader $ eitherReader (Right . splitComma),
          option,
          long "disabled-mutations",
          env "DISABLED_MUTATIONS",
          conf "disabled-mutations",
          metavar "NAME,NAME,...",
          value []
        ]
    settingIgnore <-
      setting
        [ help "Unqualified identifier names whose calls (and argument subtrees) are left untouched",
          reader $ eitherReader (Right . splitComma),
          option,
          long "ignore",
          env "IGNORE",
          conf "ignore",
          metavar "NAME,NAME,...",
          value []
        ]
    settingSkipThSplices <-
      yesNoSwitch
        [ help "Skip mutations inside Template Haskell splices and quasi-quotes",
          long "skip-th-splices",
          env "SKIP_TH_SPLICES",
          conf "skip-th-splices",
          value True
        ]
    settingDebug <-
      yesNoSwitch
        [ help "Print each mutation site as it is recorded",
          long "debug",
          env "DEBUG",
          conf "debug",
          value False
        ]
    pure Settings {..}

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
  (w, []) -> [w]
  (w, _ : rest) -> w : splitComma rest
