{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.OptParse where

import Autodocodec
import Autodocodec.Yaml
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Env
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import Path
import Path.IO
import System.Exit
import Test.Syd.Run
import Text.Colour

#ifdef mingw32_HOST_OS
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (stdout)
#else
import Text.Colour.Capabilities.FromEnv
#endif

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfiguration flags env
  combineToSettings flags env config

-- | Test suite definition and run settings
data Settings = Settings
  { -- | The seed to use for deterministic randomness
    settingSeed :: !SeedSetting,
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
    -- | The filters to use to select which tests to run
    settingFilters :: ![Text],
    -- | Whether to stop upon the first test failure
    settingFailFast :: !Bool,
    -- | How many iterations to use to look diagnose flakiness
    settingIterations :: !Iterations,
    -- | How many times to retry a test for flakiness diagnostics
    settingRetries :: !Word,
    -- | Whether to fail when any flakiness is detected in tests declared as flaky
    settingFailOnFlaky :: !Bool,
    -- | How to report progress
    settingReportProgress :: !ReportProgress,
    -- | Debug mode
    settingDebug :: !Bool,
    -- | Profiling mode
    settingProfile :: !Bool
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
          settingFilters = mempty,
          settingFailFast = False,
          settingIterations = OneIteration,
          settingRetries = defaultRetries,
          settingFailOnFlaky = False,
          settingReportProgress = ReportNoProgress,
          settingDebug = False,
          settingProfile = False
        }

defaultRetries :: Word
defaultRetries = 3

deriveTerminalCapababilities :: Settings -> IO TerminalCapabilities
deriveTerminalCapababilities settings = case settingColour settings of
  Just False -> pure WithoutColours
  Just True -> pure With8BitColours
  Nothing -> detectTerminalCapabilities

#ifdef mingw32_HOST_OS
detectTerminalCapabilities :: IO TerminalCapabilities
detectTerminalCapabilities = do
  supports <- hSupportsANSIColor stdout
  if supports
    then pure With8BitColours
    else pure WithoutColours
#else
detectTerminalCapabilities :: IO TerminalCapabilities
detectTerminalCapabilities = getTerminalCapabilitiesFromEnv
#endif

data Threads
  = -- | One thread
    Synchronous
  | -- | As many threads as 'getNumCapabilities' tells you you have
    ByCapabilities
  | -- | A given number of threads
    Asynchronous !Word
  deriving (Show, Read, Eq, Generic)

data Iterations
  = -- | Run the test suite once, the default
    OneIteration
  | -- | Run the test suite for the given number of iterations, or until we can find flakiness
    Iterations !Word
  | -- | Run the test suite over and over, until we can find some flakiness
    Continuous
  deriving (Show, Read, Eq, Generic)

data ReportProgress
  = -- | Don't report any progress, the default
    ReportNoProgress
  | -- | Report progress
    ReportProgress
  deriving (Show, Read, Eq, Generic)

-- | Combine everything to 'Settings'
combineToSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
combineToSettings Flags {..} Environment {..} mConf = do
  let d func = func defaultSettings
  let debugMode =
        fromMaybe (d settingDebug) $
          flagDebug <|> envDebug <|> mc configDebug
  let threads =
        fromMaybe (if debugMode then Synchronous else d settingThreads) $
          flagThreads <|> envThreads <|> mc configThreads
  setReportProgress <-
    case flagReportProgress <|> envReportProgress <|> mc configReportProgress of
      Nothing ->
        pure $
          if threads == Synchronous
            then
              if debugMode
                then ReportProgress
                else d settingReportProgress
            else d settingReportProgress
      Just progress ->
        if progress
          then
            if threads /= Synchronous
              then die "Reporting progress in asynchronous runners is not supported. You can use --synchronous or --debug to use a synchronous runner."
              else pure ReportProgress
          else pure ReportNoProgress

  pure
    Settings
      { settingSeed =
          fromMaybe (d settingSeed) $
            flagSeed <|> envSeed <|> mc configSeed,
        settingRandomiseExecutionOrder =
          fromMaybe (if debugMode then False else d settingRandomiseExecutionOrder) $
            flagRandomiseExecutionOrder <|> envRandomiseExecutionOrder <|> mc configRandomiseExecutionOrder,
        settingThreads = threads,
        settingMaxSuccess =
          fromMaybe (d settingMaxSuccess) $
            flagMaxSuccess <|> envMaxSuccess <|> mc configMaxSuccess,
        settingMaxSize =
          fromMaybe (d settingMaxSize) $
            flagMaxSize <|> envMaxSize <|> mc configMaxSize,
        settingMaxDiscard =
          fromMaybe (d settingMaxDiscard) $
            flagMaxDiscard <|> envMaxDiscard <|> mc configMaxDiscard,
        settingMaxShrinks =
          fromMaybe (d settingMaxShrinks) $
            flagMaxShrinks <|> envMaxShrinks <|> mc configMaxShrinks,
        settingGoldenStart =
          fromMaybe (d settingGoldenStart) $
            flagGoldenStart <|> envGoldenStart <|> mc configGoldenStart,
        settingGoldenReset =
          fromMaybe (d settingGoldenReset) $
            flagGoldenReset <|> envGoldenReset <|> mc configGoldenReset,
        settingColour = flagColour <|> envColour <|> mc configColour,
        settingFilters = flagFilters <|> maybeToList envFilter <|> maybeToList (mc configFilter),
        settingFailFast =
          fromMaybe
            (if debugMode then True else d settingFailFast)
            (flagFailFast <|> envFailFast <|> mc configFailFast),
        settingIterations =
          fromMaybe (d settingIterations) $
            flagIterations <|> envIterations <|> mc configIterations,
        settingRetries =
          fromMaybe (if debugMode then 0 else d settingRetries) $
            flagRetries <|> envRetries <|> mc configRetries,
        settingFailOnFlaky =
          fromMaybe (d settingFailOnFlaky) $
            flagFailOnFlaky <|> envFailOnFlaky <|> mc configFailOnFlaky,
        settingReportProgress = setReportProgress,
        settingDebug = debugMode,
        settingProfile =
          fromMaybe False $
            flagProfile <|> envProfile <|> mc configProfile
      }
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the configuration file.
-- For example, use 'Maybe FilePath', not 'Path Abs File'.
--
-- Use 'readYamlConfigFile' or 'readFirstYamlConfigFile' to read a configuration.
data Configuration = Configuration
  { configSeed :: !(Maybe SeedSetting),
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
    configFailFast :: !(Maybe Bool),
    configIterations :: !(Maybe Iterations),
    configRetries :: !(Maybe Word),
    configFailOnFlaky :: !(Maybe Bool),
    configReportProgress :: !(Maybe Bool),
    configDebug :: !(Maybe Bool),
    configProfile :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

-- | We use 'autodocodec' for parsing a YAML config.
instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "seed" "Seed for random generation of test cases"
          .= configSeed
        <*> parseAlternative
          (optionalField "randomise-execution-order" "Randomise the execution order of the tests in the test suite")
          (optionalField "randomize-execution-order" "American spelling")
          .= configRandomiseExecutionOrder
        <*> optionalField "parallelism" "How parallel to execute the tests"
          .= configThreads
        <*> optionalField "max-size" "Maximum size parameter to pass to generators"
          .= configMaxSize
        <*> optionalField "max-success" "Number of quickcheck examples to run"
          .= configMaxSuccess
        <*> optionalField "max-discard" "Maximum number of discarded tests per successful test before giving up"
          .= configMaxDiscard
        <*> optionalField "max-shrinks" "Maximum number of shrinks of a failing test input"
          .= configMaxShrinks
        <*> optionalField "golden-start" "Whether to write golden tests if they do not exist yet"
          .= configGoldenStart
        <*> optionalField "golden-reset" "Whether to overwrite golden tests instead of having them fail"
          .= configGoldenReset
        <*> parseAlternative
          (optionalField "colour" "Whether to use coloured output")
          (optionalField "color" "American spelling")
          .= configColour
        <*> optionalField "filter" "Filter to select which parts of the test tree to run"
          .= configFilter
        <*> optionalField "fail-fast" "Whether to stop executing upon the first test failure"
          .= configFailFast
        <*> optionalField "iterations" "How many iterations to use to look diagnose flakiness"
          .= configIterations
        <*> optionalField "retries" "The number of retries to use for flakiness diagnostics. 0 means 'no flakiness diagnostics'"
          .= configRetries
        <*> optionalField "fail-on-flaky" "Whether to fail when any flakiness is detected in tests marked as potentially flaky"
          .= configFailOnFlaky
        <*> optionalField "progress" "How to report progres"
          .= configReportProgress
        <*> optionalField "debug" "Turn on debug-mode. This implies randomise-execution-order: false, parallelism: 1 and fail-fast: true"
          .= configDebug
        <*> optionalField "profile" "Turn on profiling mode"
          .= configProfile

instance HasCodec Threads where
  codec = dimapCodec f g codec
    where
      f = \case
        Nothing -> ByCapabilities
        Just 1 -> Synchronous
        Just n -> Asynchronous n
      g = \case
        ByCapabilities -> Nothing
        Synchronous -> Just 1
        Asynchronous n -> Just n

instance HasCodec Iterations where
  codec = dimapCodec f g codec
    where
      f = \case
        Nothing -> OneIteration
        Just 0 -> Continuous
        Just 1 -> OneIteration
        Just n -> Iterations n
      g = \case
        OneIteration -> Nothing
        Continuous -> Just 0
        Iterations n -> Just n

-- | Get the configuration
--
-- We use the flags and environment because they can contain information to override where to look for the configuration files.
-- We return a 'Maybe' because there may not be a configuration file.
getConfiguration :: Flags -> Environment -> IO (Maybe Configuration)
getConfiguration Flags {..} Environment {..} =
  case flagConfigFile <|> envConfigFile of
    Nothing -> defaultConfigFile >>= readYamlConfigFile
    Just cf -> do
      afp <- resolveFile' cf
      readYamlConfigFile afp

-- | Where to get the configuration file by default.
defaultConfigFile :: IO (Path Abs File)
defaultConfigFile = resolveFile' ".sydtest.yaml"

-- | What we find in the configuration variable.
--
-- Do nothing clever here, just represent the relevant parts of the environment.
-- For example, use 'Text', not 'SqliteConfig'.
data Environment = Environment
  { envConfigFile :: Maybe FilePath,
    envSeed :: !(Maybe SeedSetting),
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
    envFailFast :: !(Maybe Bool),
    envIterations :: !(Maybe Iterations),
    envRetries :: !(Maybe Word),
    envFailOnFlaky :: !(Maybe Bool),
    envReportProgress :: !(Maybe Bool),
    envDebug :: !(Maybe Bool),
    envProfile :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

defaultEnvironment :: Environment
defaultEnvironment =
  Environment
    { envConfigFile = Nothing,
      envSeed = Nothing,
      envRandomiseExecutionOrder = Nothing,
      envThreads = Nothing,
      envMaxSize = Nothing,
      envMaxSuccess = Nothing,
      envMaxDiscard = Nothing,
      envMaxShrinks = Nothing,
      envGoldenStart = Nothing,
      envGoldenReset = Nothing,
      envColour = Nothing,
      envFilter = Nothing,
      envFailFast = Nothing,
      envIterations = Nothing,
      envRetries = Nothing,
      envFailOnFlaky = Nothing,
      envReportProgress = Nothing,
      envDebug = Nothing,
      envProfile = Nothing
    }

getEnvironment :: IO Environment
getEnvironment = Env.parse (Env.header "Environment") environmentParser

-- | The 'envparse' parser for the 'Environment'
environmentParser :: Env.Parser Env.Error Environment
environmentParser =
  Env.prefixed "SYDTEST_" $
    Environment
      <$> Env.var (fmap Just . Env.str) "CONFIG_FILE" (Env.def Nothing <> Env.help "Config file")
      <*> seedSettingEnvironmentParser
      <*> ( Env.var (fmap Just . Env.auto) "RANDOMISE_EXECUTION_ORDER" (Env.def Nothing <> Env.help "Randomise the execution order of the tests in the test suite")
              <|> Env.var (fmap Just . Env.auto) "RANDOMIZE_EXECUTION_ORDER" (Env.def Nothing <> Env.help "Randomize the execution order of the tests in the test suite")
          )
      <*> Env.var (fmap Just . (Env.auto >=> parseThreads)) "PARALLELISM" (Env.def Nothing <> Env.help "How parallel to execute the tests")
      <*> Env.var (fmap Just . Env.auto) "MAX_SIZE" (Env.def Nothing <> Env.help "Maximum size parameter to pass to generators")
      <*> Env.var (fmap Just . Env.auto) "MAX_SUCCESS" (Env.def Nothing <> Env.help "Number of quickcheck examples to run")
      <*> Env.var (fmap Just . Env.auto) "MAX_DISCARD" (Env.def Nothing <> Env.help "Maximum number of discarded tests per successful test before giving up")
      <*> Env.var (fmap Just . Env.auto) "MAX_SHRINKS" (Env.def Nothing <> Env.help "Maximum number of shrinks of a failing test input")
      <*> Env.var (fmap Just . Env.auto) "GOLDEN_START" (Env.def Nothing <> Env.help "Whether to write golden tests if they do not exist yet")
      <*> Env.var (fmap Just . Env.auto) "GOLDEN_RESET" (Env.def Nothing <> Env.help "Whether to overwrite golden tests instead of having them fail")
      <*> ( Env.var (fmap Just . Env.auto) "COLOUR" (Env.def Nothing <> Env.help "Whether to use coloured output")
              <|> Env.var (fmap Just . Env.auto) "COLOR" (Env.def Nothing <> Env.help "Whether to use colored output")
          )
      <*> Env.var (fmap Just . Env.str) "FILTER" (Env.def Nothing <> Env.help "Filter to select which parts of the test tree to run")
      <*> Env.var (fmap Just . Env.auto) "FAIL_FAST" (Env.def Nothing <> Env.help "Whether to stop executing upon the first test failure")
      <*> Env.var (fmap Just . (Env.auto >=> parseIterations)) "ITERATIONS" (Env.def Nothing <> Env.help "How many iterations to use to look diagnose flakiness")
      <*> Env.var (fmap Just . Env.auto) "RETRIES" (Env.def Nothing <> Env.help "The number of retries to use for flakiness diagnostics. 0 means 'no flakiness diagnostics'")
      <*> Env.var (fmap Just . Env.auto) "FAIL_ON_FLAKY" (Env.def Nothing <> Env.help "Whether to fail when flakiness is detected in tests marked as potentially flaky")
      <*> Env.var (fmap Just . Env.auto) "PROGRESS" (Env.def Nothing <> Env.help "Report progress as tests run")
      <*> Env.var (fmap Just . Env.auto) "DEBUG" (Env.def Nothing <> Env.help "Turn on debug mode. This implies RANDOMISE_EXECUTION_ORDER=False, PARALLELISM=1 and FAIL_FAST=True.")
      <*> Env.var (fmap Just . Env.auto) "PROFILE" (Env.def Nothing <> Env.help "Turn on profiling mode.")
  where
    parseThreads :: Word -> Either e Threads
    parseThreads 1 = Right Synchronous
    parseThreads i = Right (Asynchronous i)
    parseIterations :: Word -> Either e Iterations
    parseIterations 0 = Right Continuous
    parseIterations 1 = Right OneIteration
    parseIterations i = Right (Iterations i)

seedSettingEnvironmentParser :: Env.Parser Env.Error (Maybe SeedSetting)
seedSettingEnvironmentParser =
  combine
    <$> Env.var (fmap Just . Env.auto) "SEED" (Env.def Nothing <> Env.help "Seed for random generation of test cases")
    <*> Env.switch "RANDOM_SEED" (Env.help "Use a random seed for every test case")
  where
    combine :: Maybe Int -> Bool -> Maybe SeedSetting
    combine mSeed random = if random then Just RandomSeed else FixedSeed <$> mSeed

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
    (OptParse.fullDesc <> OptParse.footerDoc (Just $ fromString footerStr))
  where
    -- Show the variables from the environment that we parse and the config file format
    footerStr =
      unlines
        [ Env.helpDoc environmentParser,
          "",
          "Configuration file format:",
          T.unpack (renderColouredSchemaViaCodec @Configuration)
        ]

-- | The flags that are common across commands.
data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagSeed :: !(Maybe SeedSetting),
    flagRandomiseExecutionOrder :: !(Maybe Bool),
    flagThreads :: !(Maybe Threads),
    flagMaxSize :: !(Maybe Int),
    flagMaxSuccess :: !(Maybe Int),
    flagMaxDiscard :: !(Maybe Int),
    flagMaxShrinks :: !(Maybe Int),
    flagGoldenStart :: !(Maybe Bool),
    flagGoldenReset :: !(Maybe Bool),
    flagColour :: !(Maybe Bool),
    flagFilters :: ![Text],
    flagFailFast :: !(Maybe Bool),
    flagIterations :: !(Maybe Iterations),
    flagRetries :: !(Maybe Word),
    flagFailOnFlaky :: !(Maybe Bool),
    flagReportProgress :: !(Maybe Bool),
    flagDebug :: !(Maybe Bool),
    flagProfile :: !(Maybe Bool)
  }
  deriving (Show, Eq, Generic)

defaultFlags :: Flags
defaultFlags =
  Flags
    { flagConfigFile = Nothing,
      flagSeed = Nothing,
      flagRandomiseExecutionOrder = Nothing,
      flagThreads = Nothing,
      flagMaxSize = Nothing,
      flagMaxSuccess = Nothing,
      flagMaxDiscard = Nothing,
      flagMaxShrinks = Nothing,
      flagGoldenStart = Nothing,
      flagGoldenReset = Nothing,
      flagColour = Nothing,
      flagFilters = mempty,
      flagFailFast = Nothing,
      flagIterations = Nothing,
      flagRetries = Nothing,
      flagFailOnFlaky = Nothing,
      flagReportProgress = Nothing,
      flagDebug = Nothing,
      flagProfile = Nothing
    }

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
    <*> seedSettingFlags
    <*> doubleSwitch ["randomise-execution-order", "randomize-execution-order"] (help "Randomise the execution order of the tests in the test suite")
    <*> optional
      ( ( ( \case
              1 -> Synchronous
              i -> Asynchronous i
          )
            <$> option
              auto
              ( mconcat
                  [ short 'j',
                    long "jobs",
                    help "How parallel to execute the tests",
                    metavar "JOBS"
                  ]
              )
        )
          <|> flag'
            Synchronous
            ( mconcat
                [ long "synchronous",
                  help "Execute tests synchronously"
                ]
            )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-size",
                long "qc-max-size",
                help "Maximum size parameter to pass to generators",
                metavar "MAXIMUM_SIZE_PARAMETER"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-success",
                long "qc-max-success",
                help "Number of quickcheck examples to run",
                metavar "NUMBER_OF_SUCCESSES"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-discard",
                long "qc-max-discard",
                help "Maximum number of discarded tests per successful test before giving up",
                metavar "MAXIMUM_DISCARD_RATIO"
              ]
          )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "max-shrinks",
                long "qc-max-shrinks",
                help "Maximum number of shrinks of a failing test input",
                metavar "MAXIMUM_SHRINKS"
              ]
          )
      )
    <*> doubleSwitch ["golden-start"] (help "Whether to write golden tests if they do not exist yet")
    <*> doubleSwitch ["golden-reset"] (help "Whether to overwrite golden tests instead of having them fail")
    <*> doubleSwitch ["colour", "color"] (help "Use colour in output")
    <*> ( maybeToList
            <$> optional
              ( strArgument
                  ( mconcat
                      [ help "Filter to select which parts of the test tree to run",
                        metavar "FILTER"
                      ]
                  )
              )
            <|> manyOptional
              ( mconcat
                  [ short 'f',
                    long "filter",
                    short 'm',
                    long "match",
                    help "Filter to select which parts of the test tree to run",
                    metavar "FILTER"
                  ]
              )
        )
    <*> doubleSwitch ["fail-fast"] (help "Stop upon the first test failure")
    <*> optional
      ( ( ( \case
              0 -> Continuous
              1 -> OneIteration
              i -> Iterations i
          )
            <$> option
              auto
              ( mconcat
                  [ long "iterations",
                    help "How many iterations to use to look diagnose flakiness",
                    metavar "ITERATIONS"
                  ]
              )
        )
          <|> flag'
            Continuous
            ( mconcat
                [ long "continuous",
                  help "Run the test suite over and over again until it fails, to diagnose flakiness"
                ]
            )
      )
    <*> optional
      ( option
          auto
          ( mconcat
              [ long "retries",
                help "The number of retries to use for flakiness diagnostics. 0 means 'no flakiness diagnostics'",
                metavar "INTEGER"
              ]
          )
      )
    <*> doubleSwitch ["fail-on-flaky"] (help "Fail when any flakiness is detected")
    <*> doubleSwitch ["progress"] (help "Report progress")
    <*> doubleSwitch ["debug"] (help "Turn on debug mode. This implies --no-randomise-execution-order, --synchronous, --progress and --fail-fast.")
    <*> doubleSwitch ["profile"] (help "Turn on profiling mode.")

manyOptional :: OptParse.Mod OptionFields Text -> OptParse.Parser [Text]
manyOptional modifier = many (option str modifier)

seedSettingFlags :: OptParse.Parser (Maybe SeedSetting)
seedSettingFlags =
  optional $
    ( FixedSeed
        <$> option
          auto
          ( mconcat
              [ long "seed",
                help "Seed for random generation of test cases",
                metavar "SEED"
              ]
          )
    )
      <|> flag'
        RandomSeed
        ( mconcat
            [ long "random-seed",
              help "Use a random seed instead of a fixed seed"
            ]
        )

doubleSwitch :: [String] -> OptParse.Mod FlagFields (Maybe Bool) -> OptParse.Parser (Maybe Bool)
doubleSwitch suffixes mods =
  flag' (Just True) (hidden <> internal <> foldMap long suffixes <> mods)
    <|> flag' (Just False) (hidden <> internal <> foldMap (long . ("no-" <>)) suffixes <> mods)
    <|> flag' Nothing (foldMap (\suffix -> long ("[no-]" <> suffix)) suffixes <> mods)
    <|> pure Nothing
