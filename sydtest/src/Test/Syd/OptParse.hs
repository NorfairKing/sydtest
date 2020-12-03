{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
    -- | The filter to use to select which tests to run
    settingFilter :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

defaultSettings :: Settings
defaultSettings =
  let d func = func defaultTestRunSettings
   in Settings
        { settingSeed = d testRunSettingSeed,
          settingThreads = ByCapabilities,
          settingMaxSuccess = d testRunSettingMaxSuccess,
          settingMaxSize = d testRunSettingMaxSize,
          settingMaxDiscard = d testRunSettingMaxDiscardRatio,
          settingMaxShrinks = d testRunSettingMaxShrinks,
          settingFilter = Nothing
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
        settingThreads = fromMaybe (d settingThreads) $ flagThreads <|> envThreads <|> mc configThreads,
        settingMaxSuccess = fromMaybe (d settingMaxSuccess) $ flagMaxSuccess <|> envMaxSuccess <|> mc configMaxSuccess,
        settingMaxSize = fromMaybe (d settingMaxSize) $ flagMaxSize <|> envMaxSize <|> mc configMaxSize,
        settingMaxDiscard = fromMaybe (d settingMaxDiscard) $ flagMaxDiscard <|> envMaxDiscard <|> mc configMaxDiscard,
        settingMaxShrinks = fromMaybe (d settingMaxShrinks) $ flagMaxShrinks <|> envMaxShrinks <|> mc configMaxShrinks,
        settingFilter = flagFilter <|> envFilter <|> mc configFilter
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
    configThreads :: !(Maybe Threads),
    configMaxSize :: !(Maybe Int),
    configMaxSuccess :: !(Maybe Int),
    configMaxDiscard :: !(Maybe Int),
    configMaxShrinks :: !(Maybe Int),
    configFilter :: !(Maybe Text)
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
        <*> optionalField "parallelism" "How parallel to execute the tests"
        <*> optionalField "max-success" "Number of quickcheck examples to run"
        <*> optionalField "max-size" "Maximum size parameter to pass to generators"
        <*> optionalField "max-discard" "Maximum number of discarded tests per successful test before giving up"
        <*> optionalField "max-shrinks" "Maximum number of shrinks of a failing test input"
        <*> optionalField "filter" "Filter to select which parts of the test tree to run"

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
--
-- This uses the XDG base directory specifictation:
-- https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = do
  xdgConfigDir <- getXdgDir XdgConfig (Just [reldir|optparse-template|])
  resolveFile xdgConfigDir "config.yaml"

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the relevant parts of the environment.
-- For example, use 'Text', not 'SqliteConfig'.
data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envSeed :: !(Maybe Int),
    envThreads :: !(Maybe Threads),
    envMaxSize :: !(Maybe Int),
    envMaxSuccess :: !(Maybe Int),
    envMaxDiscard :: !(Maybe Int),
    envMaxShrinks :: !(Maybe Int),
    envFilter :: !(Maybe Text)
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
      <*> Env.var (fmap Just . (Env.auto >=> parseThreads)) "PARALLELISM" (mE <> Env.help "How parallel to execute the tests")
      <*> Env.var (fmap Just . Env.auto) "MAX_SUCCESS" (mE <> Env.help "Number of quickcheck examples to run")
      <*> Env.var (fmap Just . Env.auto) "MAX_SIZE" (mE <> Env.help "Maximum size parameter to pass to generators")
      <*> Env.var (fmap Just . Env.auto) "MAX_DISCARD" (mE <> Env.help "Maximum number of discarded tests per successful test before giving up")
      <*> Env.var (fmap Just . Env.auto) "MAX_SHRINKS" (mE <> Env.help "Maximum number of shrinks of a failing test input")
      <*> Env.var (fmap Just . Env.str) "FILTER" (mE <> Env.help "Filter to select which parts of the test tree to run")
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
  { flagConfigFile :: Maybe FilePath,
    flagSeed :: Maybe Int,
    flagThreads :: Maybe Threads,
    flagMaxSuccess :: Maybe Int,
    flagMaxSize :: Maybe Int,
    flagMaxDiscard :: Maybe Int,
    flagMaxShrinks :: Maybe Int,
    flagFilter :: Maybe Text
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
      <*> ( optional
              ( option
                  auto
                  ( mconcat
                      [ long "seed",
                        help "Seed for random generation of test cases"
                      ]
                  )
              )
          )
      <*> optional
        ( ( \case
              1 -> Synchronous
              i -> Asynchronous i
          )
            <$> option auto (mconcat [short 'j', long "jobs", help "How parallel to execute the tests"])
        )
      <*> ( optional
              ( option
                  auto
                  ( mconcat
                      [ long "max-success",
                        help "Number of quickcheck examples to run"
                      ]
                  )
              )
          )
      <*> ( optional
              ( option
                  auto
                  ( mconcat
                      [ long "max-size",
                        help "Maximum size parameter to pass to generators"
                      ]
                  )
              )
          )
      <*> ( optional
              ( option
                  auto
                  ( mconcat
                      [ long "max-discard",
                        help "Maximum number of discarded tests per successful test before giving up"
                      ]
                  )
              )
          )
      <*> ( optional
              ( option
                  auto
                  ( mconcat
                      [ long "max-shrinks",
                        help "Maximum number of shrinks of a failing test input"
                      ]
                  )
              )
          )
      <*> ( optional
              ( strOption
                  ( mconcat
                      [ long "filter",
                        long "match",
                        help "Filter to select which parts of the test tree to run"
                      ]
                  )
              )
          )
