{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.Mutation.Plugin.OptParse
  ( Settings (..),
    OperatorConfig (..),
    operatorsConfigDisabled,
    operatorExtraFlag,
    defaultSettings,
    parseSettings,
    resolveSettings,
  )
where

import Data.Aeson (Object, Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
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
    -- | Per-operator configuration, keyed by operator name, read from the
    -- @operators@ config object.
    settingOperators :: !(Map Text OperatorConfig),
    -- | Print each mutation site as it is recorded.
    settingDebug :: !Bool
  }
  deriving (Show, Eq, Generic)

-- | Configuration for a single mutation operator, read from one entry of
-- the @operators@ config object:
--
-- > operators:
-- >   ConstEmptyList:
-- >     enable: true
-- >     skip-strings: true
-- >   Arith:
-- >     enable: false
--
-- @enable@ is common to every operator; the remaining keys are an opaque
-- 'operatorConfigExtra' blob that each operator interprets itself (e.g.
-- @ConstEmptyList@ reads @skip-strings@).  An operator absent from the
-- config behaves as @enable: true@ with no extra.
data OperatorConfig = OperatorConfig
  { -- | Whether the operator runs.  @enable: false@ disables it exactly as
    -- if its name were in @disabled-mutations@.
    operatorConfigEnable :: !Bool,
    -- | Operator-specific config keys (everything under the operator other
    -- than @enable@), interpreted by the operator.
    operatorConfigExtra :: !(Map Text Value)
  }
  deriving (Show, Eq, Generic)

-- | Names of operators that 'OperatorConfig' explicitly disables.
operatorsConfigDisabled :: Map Text OperatorConfig -> [String]
operatorsConfigDisabled m =
  [T.unpack opName | (opName, cfg) <- Map.toList m, not (operatorConfigEnable cfg)]

-- | Read a boolean flag from an operator's 'operatorConfigExtra', defaulting
-- to 'False'.  A small helper for operators interpreting their own config.
operatorExtraFlag :: Text -> Map Text Value -> Bool
operatorExtraFlag k m = case Map.lookup k m of
  Just (Bool b) -> b
  _ -> False

-- | Decode the raw @operators@ config object into a per-operator map.
decodeOperatorsConfig :: Object -> Map Text OperatorConfig
decodeOperatorsConfig obj =
  Map.fromList
    [(Key.toText k, decodeOperatorConfig v) | (k, v) <- KeyMap.toList obj]
  where
    decodeOperatorConfig :: Value -> OperatorConfig
    decodeOperatorConfig = \case
      Object o ->
        OperatorConfig
          { operatorConfigEnable = case KeyMap.lookup "enable" o of
              Just (Bool b) -> b
              _ -> True,
            operatorConfigExtra =
              Map.fromList
                [(Key.toText k, val) | (k, val) <- KeyMap.toList o, k /= "enable"]
          }
      _ -> OperatorConfig {operatorConfigEnable = True, operatorConfigExtra = Map.empty}

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
      settingOperators = Map.empty,
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
    settingOperators <-
      maybe Map.empty decodeOperatorsConfig
        <$> optional
          ( setting
              [ help "Per-operator config object: operators.<Name>.{enable, <operator-specific keys>}",
                conf "operators"
              ]
          )
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
