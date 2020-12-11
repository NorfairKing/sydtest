{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.OptParse where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import qualified Options.Applicative.Help as OptParse (string)
import Path
import Path.IO
import Test.Syd.Run
import YamlParse.Applicative as YamlParse

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

-- | Test suite definition and run settings
data Settings = Settings
  { -- | The seed to use for deterministic randomness
    settingSeed :: !Int,
    -- | Randomise the execution order of the tests in the test suite
    settingRandomiseExecutionOrder :: !Bool,
    -- | How parallel to run the test suite
    settingThreads :: !Threads,
    -- | How many examples to run a property test with
    settingMaxSuccess :: !Int,
    -- | The maximum size parameter to supply to generators
    settingMaxSize :: !Int,
    -- | The maximum number of discarded examples per tested example
    settingMaxDiscard :: !Int,
    -- | The maximum number of tries to use while shrinking a counterexample.
    settingMaxShrinks :: !Int,
    -- | Whether to write golden tests if they do not exist yet
    settingGoldenStart :: !Bool,
    -- | Whether to overwrite golden tests instead of having them fail
    settingGoldenReset :: !Bool,
    -- | Whether to use colour in the output
    settingColour :: !(Maybe Bool),
    -- | The filter to use to select which tests to run
    settingFilter :: !(Maybe Text),
    -- | Whether to stop upon the first test failure
    settingFailFast :: !Bool
  }
  deriving (Show, Eq, Generic)

defaultSettings :: Settings
defaultSettings =
  let d func = func defaultTestRunSettings
   in Settings
        { settingSeed = d testRunSettingSeed,
          settingRandomiseExecutionOrder = True,
          settingThreads = ByCapabilities,
          settingMaxSuccess = d testRunSettingMaxSuccess,
          settingMaxSize = d testRunSettingMaxSize,
          settingMaxDiscard = d testRunSettingMaxDiscardRatio,
          settingMaxShrinks = d testRunSettingMaxShrinks,
          settingGoldenStart = d testRunSettingGoldenStart,
          settingGoldenReset = d testRunSettingGoldenReset,
          settingColour = Nothing,
          settingFilter = Nothing,
          settingFailFast = False
        }

data Threads
  = -- | One thread
    Synchronous
  | -- | As many threads as 'getNumCapabilities' tells you you have
    ByCapabilities
  | -- | A given number of threads
    Asynchronous Int
  deriving (Show, Eq, Generic)

-- | Combine everything to 'Settings'
combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let d func = func defaultSettings
  pure
    Settings
      { settingSeed = fromMaybe (d settingSeed) $ flagSeed <|> envSeed <|> mc configSeed,
        settingRandomiseExecutionOrder = fromMaybe (d settingRandomiseExecutionOrder) $ flagRandomiseExecutionOrder <|> envRandomiseExecutionOrder <|> mc configRandomiseExecutionOrder,
        settingThreads = fromMaybe (d settingThreads) $ flagThreads <|> envThreads <|> mc configThreads,
        settingMaxSuccess = fromMaybe (d settingMaxSuccess) $ flagMaxSuccess <|> envMaxSuccess <|> mc configMaxSuccess,
        settingMaxSize = fromMaybe (d settingMaxSize) $ flagMaxSize <|> envMaxSize <|> mc configMaxSize,
        settingMaxDiscard = fromMaybe (d settingMaxDiscard) $ flagMaxDiscard <|> envMaxDiscard <|> mc configMaxDiscard,
        settingMaxShrinks = fromMaybe (d settingMaxShrinks) $ flagMaxShrinks <|> envMaxShrinks <|> mc configMaxShrinks,
        settingGoldenStart = fromMaybe (d settingGoldenStart) $ flagGoldenStart <|> envGoldenStart <|> mc configGoldenStart,
        settingGoldenReset = fromMaybe (d settingGoldenReset) $ flagGoldenReset <|> envGoldenReset <|> mc configGoldenReset,
        settingColour = flagColour <|> envColour <|> mc configColour,
        settingFilter = flagFilter <|> envFilter <|> mc configFilter,
        settingFailFast = fromMaybe (d settingFailFast) $ flagFailFast <|> envFailFast <|> mc configFailFast
      }
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the configuration file.
-- For example, use 'Maybe FilePath', not 'Path Abs File'.
--
-- Use 'YamlParse.readConfigFile' or 'YamlParse.readFirstConfigFile' to read a configuration.
data Configuration = Configuration
  { configSeed :: !(Maybe Int),
    configRandomiseExecutionOrder :: !(Maybe Bool),
    configThreads :: !(Maybe Threads),
    configMaxSize :: !(Maybe Int),
    configMaxSuccess :: !(Maybe Int),
    configMaxDiscard :: !(Maybe Int),
    configMaxShrinks :: !(Maybe Int),
    configGoldenStart :: !(Maybe Bool),
    configGoldenReset :: !(Maybe Bool),
    configColour :: !(Maybe Bool),
    configFilter :: !(Maybe Text),
    configFailFast :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

-- | We use 'yamlparse-applicative' for parsing a YAML config.
instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalField "seed" "Seed for random generation of test cases"
        <*> optionalField "randomise-execution-order" "Randomise the execution order of the tests in the test suite"
        <*> optionalField "parallelism" "How parallel to execute the tests"
        <*> optionalField "max-success" "Number of quickcheck examples to run"
        <*> optionalField "max-size" "Maximum size parameter to pass to generators"
        <*> optionalField "max-discard" "Maximum number of discarded tests per successful test before giving up"
        <*> optionalField "max-shrinks" "Maximum number of shrinks of a failing test input"
        <*> optionalField "golden-start" "Whether to write golden tests if they do not exist yet"
        <*> optionalField "golden-reset" "Whether to overwrite golden tests instead of having them fail"
        <*> optionalField "colour" "Whether to use coloured output"
        <*> optionalField "filter" "Filter to select which parts of the test tree to run"
        <*> optionalField "fail-fast" "Whether to stop executing upon the first test failure"

instance YamlSchema Threads where
  yamlSchema = flip fmap yamlSchema $ \case
    Nothing -> ByCapabilities
    Just 1 -> Synchronous
    Just n -> Asynchronous n

-- | Get the configuration
--
-- We use the flags and environment because they can contain information to override where to look for the configuration files.
-- We return a 'Maybe' because there may not be a configuration file.
getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= YamlParse.readConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      YamlParse.readConfigFile afp

-- | Where to get the configuration file by default.
defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = resolveFile' ".sydtest.yaml"

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the relevant parts of the environment.
-- For example, use 'Text', not 'SqliteConfig'.
data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envSeed :: !(Maybe Int),
    envRandomiseExecutionOrder :: !(Maybe Bool),
    envThreads :: !(Maybe Threads),
    envMaxSize :: !(Maybe Int),
    envMaxSuccess :: !(Maybe Int),
    envMaxDiscard :: !(Maybe Int),
    envMaxShrinks :: !(Maybe Int),
    envGoldenStart :: !(Maybe Bool),
    envGoldenReset :: !(Maybe Bool),
    envColour :: !(Maybe Bool),
    envFilter :: !(Maybe Text),
    envFailFast :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "SYDTEST_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (mE <> Env.help "Config file")
        <*> Env.var (fmap Just . Env.auto) "SEED" (mE <> Env.help "Seed for random generation of test cases")
        <*> Env.var (fmap Just . Env.auto) "RANDOMISE_EXECUTION_ORDER" (mE <> Env.help "Randomise the execution order of the tests in the test suite")
        <*> Env.var (fmap Just . (Env.auto >=> parseThreads)) "PARALLELISM" (mE <> Env.help "How parallel to execute the tests")
        <*> Env.var (fmap Just . Env.auto) "MAX_SUCCESS" (mE <> Env.help "Number of quickcheck examples to run")
        <*> Env.var (fmap Just . Env.auto) "MAX_SIZE" (mE <> Env.help "Maximum size parameter to pass to generators")
        <*> Env.var (fmap Just . Env.auto) "MAX_DISCARD" (mE <> Env.help "Maximum number of discarded tests per successful test before giving up")
        <*> Env.var (fmap Just . Env.auto) "MAX_SHRINKS" (mE <> Env.help "Maximum number of shrinks of a failing test input")
        <*> Env.var (fmap Just . Env.auto) "GOLDEN_START" (mE <> Env.help "Whether to write golden tests if they do not exist yet")
        <*> Env.var (fmap Just . Env.auto) "GOLDEN_RESET" (mE <> Env.help "Whether to overwrite golden tests instead of having them fail")
        <*> Env.var (fmap Just . Env.auto) "COLOUR" (mE <> Env.help "Whether to use coloured output")
        <*> Env.var (fmap Just . Env.str) "FILTER" (mE <> Env.help "Filter to select which parts of the test tree to run")
        <*> Env.var (fmap Just . Env.auto) "FAIL_FAST" (mE <> Env.help "Whether to stop executing upon the first test failure")
  where
    parseThreads :: Int -> Either e Threads
    parseThreads 1 = Right Synchronous
    parseThreads i = Right (Asynchronous i)
    mE = Env.def Nothing

-- | Get the command-line flags
getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

-- | The 'optparse-applicative' parsing preferences
prefs_ :: OptParse.ParserPrefs
prefs_ =
  -- I like these preferences. Use what you like.
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

-- | The @optparse-applicative@ parser for 'Flags'
flagsParser :: OptParse.ParserInfo Flags
flagsParser =
  OptParse.info
    (OptParse.helper <*> parseFlags)
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ OptParse.string footerStr))
  where
    -- Show the variables from the environment that we parse and the config file format
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (YamlParse.prettyColourisedSchemaDoc @Configuration)
        ]

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagSeed :: !(Maybe Int),
    flagRandomiseExecutionOrder :: !(Maybe Bool),
    flagThreads :: !(Maybe Threads),
    flagMaxSuccess :: !(Maybe Int),
    flagMaxSize :: !(Maybe Int),
    flagMaxDiscard :: !(Maybe Int),
    flagMaxShrinks :: !(Maybe Int),
    flagGoldenStart :: !(Maybe Bool),
    flagGoldenReset :: !(Maybe Bool),
    flagColour :: !(Maybe Bool),
    flagFilter :: !(Maybe Text),
    flagFailFast :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

-- | The 'optparse-applicative' parser for the 'Flags'.
parseFlags :: OptParse.Parser Flags
parseFlags =
  Flags
    <$> optional
      ( strOption
          ( mconcat
              [ long "config-file",
                help "Path to an altenative config file",
                metavar "FILEPATH"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "seed",
                help "Seed for random generation of test cases"
              ]
          )
      )
    <*> optional
      ( flag
          True
          False
          ( mconcat
              [ long "no-randomise-execution-order",
                help "Randomise the execution order of the tests in the test suite"
              ]
          )
      )
    <*> optional
      ( ( \case
            1 -> Synchronous
            i -> Asynchronous i
        )
          <$> option auto (mconcat [short 'j', long "jobs", help "How parallel to execute the tests"])
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-success",
                help "Number of quickcheck examples to run"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-size",
                help "Maximum size parameter to pass to generators"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-discard",
                help "Maximum number of discarded tests per successful test before giving up"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-shrinks",
                help "Maximum number of shrinks of a failing test input"
              ]
          )
      )
    <*> optional
      ( flag
          True
          False
          ( mconcat
              [ long "no-golden-start",
                help "Whether to write golden tests if they do not exist yet"
              ]
          )
      )
    <*> optional
      ( flag
          False
          True
          ( mconcat
              [ long "golden-reset",
                help "Whether to overwrite golden tests instead of having them fail"
              ]
          )
      )
    <*> optional
      ( flag' True (mconcat [long "colour", long "color", help "Always use colour in output"])
          <|> flag' False (mconcat [long "no-colour", long "no-color", help "Never use colour in output"])
      )
    <*> optional
      ( strOption
          ( mconcat
              [ long "filter",
                long "match",
                help "Filter to select which parts of the test tree to run"
              ]
          )
      )
    <*> optional
      ( switch
          ( mconcat
              [ long "fail-fast",
                help "Whether to stop upon the first test failure"
              ]
          )
      )
