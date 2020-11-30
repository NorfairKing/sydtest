{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.OptParse where

import Control.Applicative
import Data.Maybe
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

-- | A product type for the settings that your program will use
data Settings = Settings
  { settingSeed :: Int,
    settingMaxSuccess :: Int,
    settingMaxSize :: Int,
    settingMaxDiscard :: Int,
    settingMaxShrinks :: Int
  }
  deriving (Show, Eq, Generic)

-- | Combine everything to 'Settings'
combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let d func = func defaultTestRunSettings
  let settingSeed = fromMaybe (d testRunSettingSeed) $ flagSeed <|> envSeed <|> mc configSeed
  let settingMaxSuccess = fromMaybe (d testRunSettingMaxSuccess) $ flagMaxSuccess <|> envMaxSuccess <|> mc configMaxSuccess
  let settingMaxSize = fromMaybe (d testRunSettingMaxSize) $ flagMaxSize <|> envMaxSize <|> mc configMaxSize
  let settingMaxDiscard = fromMaybe (d testRunSettingMaxDiscardRatio) $ flagMaxDiscard <|> envMaxDiscard <|> mc configMaxDiscard
  let settingMaxShrinks = fromMaybe (d testRunSettingMaxShrinks) $ flagMaxShrinks <|> envMaxShrinks <|> mc configMaxShrinks
  pure Settings {..}
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
  { configSeed :: Maybe Int,
    configMaxSize :: Maybe Int,
    configMaxSuccess :: Maybe Int,
    configMaxDiscard :: Maybe Int,
    configMaxShrinks :: Maybe Int
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
        <*> optionalField "max-success" "Number of quickcheck examples to run"
        <*> optionalField "max-size" "Maximum size parameter to pass to generators"
        <*> optionalField "max-discard" "Maximum number of discarded tests per successful test before giving up"
        <*> optionalField "max-shrinks" "Maximum number of shrinks of a failing test input"

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
    envSeed :: Maybe Int,
    envMaxSize :: Maybe Int,
    envMaxSuccess :: Maybe Int,
    envMaxDiscard :: Maybe Int,
    envMaxShrinks :: Maybe Int
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
      <*> Env.var (fmap Just . Env.auto) "MAX_SUCCESS" (mE <> Env.help "Number of quickcheck examples to run")
      <*> Env.var (fmap Just . Env.auto) "MAX_SIZE" (mE <> Env.help "Maximum size parameter to pass to generators")
      <*> Env.var (fmap Just . Env.auto) "MAX_DISCARD" (mE <> Env.help "Maximum number of discarded tests per successful test before giving up")
      <*> Env.var (fmap Just . Env.auto) "MAX_SHRINKS" (mE <> Env.help "Maximum number of shrinks of a failing test input")
  where
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
    flagMaxSuccess :: Maybe Int,
    flagMaxSize :: Maybe Int,
    flagMaxDiscard :: Maybe Int,
    flagMaxShrinks :: Maybe Int
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
