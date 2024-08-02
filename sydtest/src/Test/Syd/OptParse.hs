{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.OptParse where

import Autodocodec
import Control.Applicative
import Data.Maybe
import Data.Text (Text)
import GHC.Generics (Generic)
import OptEnvConf
import Path.IO
import Paths_sydtest (version)
import Test.Syd.Run
import Text.Colour

#ifdef mingw32_HOST_OS
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (stdout)
#else
import Text.Colour.Capabilities.FromEnv
#endif

getSettings :: IO Settings
getSettings = runSettingsParser version "A sydtest test suite"

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
    -- | Whether to use colour in the output, 'Nothing' means "detect"
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
    -- | Profiling mode
    settingProfile :: !Bool
  }
  deriving (Show, Eq, Generic)

instance HasParser Settings where
  settingsParser =
    subEnv_ "sydtest" $
      withConfigurableYamlConfig (runIO $ resolveFile' ".sydtest.yaml") $
        checkMapEither combine settingsParser
    where
      combine Flags {..} = do
        let d func = func defaultSettings
        let threads =
              fromMaybe
                ( if flagDebug
                    then Synchronous
                    else d settingThreads
                )
                flagThreads
        progress <- case flagReportProgress of
          Nothing ->
            pure $
              if threads == Synchronous
                then
                  if flagDebug
                    then ReportProgress
                    else d settingReportProgress
                else d settingReportProgress
          Just ReportProgress ->
            if threads /= Synchronous
              then Left "Reporting progress in asynchronous runners is not supported. You can use --synchronous or --debug to use a synchronous runner."
              else pure ReportProgress
          Just ReportNoProgress -> pure ReportNoProgress
        pure
          Settings
            { settingSeed = flagSeed,
              settingRandomiseExecutionOrder =
                fromMaybe
                  ( if flagDebug
                      then False
                      else d settingRandomiseExecutionOrder
                  )
                  flagRandomiseExecutionOrder,
              settingThreads = threads,
              settingMaxSuccess = flagMaxSuccess,
              settingMaxSize = flagMaxSize,
              settingMaxDiscard = flagMaxDiscard,
              settingMaxShrinks = flagMaxShrinks,
              settingGoldenStart = flagGoldenStart,
              settingGoldenReset = flagGoldenReset,
              settingColour = flagColour,
              settingFilters = flagFilters,
              settingFailFast =
                fromMaybe
                  ( if flagDebug
                      then True
                      else d settingFailFast
                  )
                  flagFailFast,
              settingIterations = flagIterations,
              settingRetries =
                fromMaybe
                  ( if flagDebug
                      then 0
                      else d settingRetries
                  )
                  flagRetries,
              settingFailOnFlaky = flagFailOnFlaky,
              settingReportProgress = progress,
              settingProfile = flagProfile
            }

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
-- We use an intermediate 'Flags' type so that default values can change based
-- on parse settings. For example, the default value for 'flagThreads' depends
-- on the value of 'flagDebug'.
data Flags = Flags
  { flagSeed :: !SeedSetting,
    flagRandomiseExecutionOrder :: !(Maybe Bool),
    flagThreads :: !(Maybe Threads),
    flagMaxSize :: !Int,
    flagMaxSuccess :: !Int,
    flagMaxDiscard :: !Int,
    flagMaxShrinks :: !Int,
    flagGoldenStart :: !Bool,
    flagGoldenReset :: !Bool,
    flagColour :: !(Maybe Bool),
    flagFilters :: ![Text],
    flagFailFast :: !(Maybe Bool),
    flagIterations :: !Iterations,
    flagRetries :: !(Maybe Word),
    flagFailOnFlaky :: !Bool,
    flagReportProgress :: !(Maybe ReportProgress),
    flagDebug :: !Bool,
    flagProfile :: !Bool
  }
  deriving (Show, Eq, Generic)

instance HasParser Flags where
  settingsParser = do
    flagSeed <- settingsParser
    flagRandomiseExecutionOrder <-
      optional $
        yesNoSwitch
          [ help "Run test suite in a random order",
            name "randomise-execution-order",
            name "randomize-execution-order"
          ]
    flagThreads <- optional settingsParser
    flagMaxSize <-
      setting
        [ help "Maximum size parameter to pass to generators",
          reader auto,
          name "max-size",
          metavar "Int",
          value $ settingMaxSize defaultSettings
        ]
    flagMaxSuccess <-
      setting
        [ help "Number of property test examples to run",
          reader auto,
          name "max-success",
          metavar "Int",
          value $ settingMaxSuccess defaultSettings
        ]
    flagMaxDiscard <-
      setting
        [ help "Maximum number of property test inputs to discard before considering the test failed",
          reader auto,
          name "max-discard",
          metavar "Int",
          value $ settingMaxDiscard defaultSettings
        ]
    flagMaxShrinks <-
      setting
        [ help "Maximum shrinks to try to apply to a failing property test input",
          reader auto,
          name "max-shrinks",
          metavar "Int",
          value $ settingMaxShrinks defaultSettings
        ]
    flagGoldenStart <-
      yesNoSwitch
        [ help "Produce initial golden output if it does not exist yet",
          name "golden-start",
          value $ settingGoldenStart defaultSettings
        ]
    flagGoldenReset <-
      yesNoSwitch
        [ help "Overwrite golden output",
          name "golden-reset",
          value $ settingGoldenReset defaultSettings
        ]
    flagColour <-
      optional $
        yesNoSwitch
          [ help "Use colour in output",
            name "colour",
            name "color"
          ]
    flagFilters <-
      choice
        [ some $
            setting
              [ help "Filter to select parts of the test suite",
                reader str,
                argument,
                metavar "FILTER"
              ],
          many $
            setting
              [ help "Filter to select parts of the test suite",
                reader str,
                option,
                short 'f',
                long "filter",
                short 'm',
                long "match",
                metavar "FILTER"
              ]
        ]
    flagFailFast <-
      optional $
        yesNoSwitch
          [ help "Stop testing when a test failure occurs",
            name "fail-fast"
          ]
    flagIterations <- settingsParser
    flagRetries <-
      optional $
        setting
          [ help "The number of retries to use for flakiness diagnostics. 0 means 'no retries'",
            reader auto,
            name "retries",
            metavar "INTEGER"
          ]
    flagFailOnFlaky <-
      yesNoSwitch
        [ help "Fail when any flakiness is detected, even when flakiness is allowed",
          name "fail-on-flaky",
          value $ settingFailOnFlaky defaultSettings
        ]
    flagReportProgress <-
      optional settingsParser
    flagDebug <-
      yesNoSwitch
        [ help "Turn on debug mode",
          name "debug",
          value False
        ]
    flagProfile <-
      yesNoSwitch
        [ help "Turn on profiling mode",
          name "profile",
          value $ settingProfile defaultSettings
        ]
    pure Flags {..}

data Threads
  = -- | One thread
    Synchronous
  | -- | As many threads as 'getNumCapabilities' tells you you have
    ByCapabilities
  | -- | A given number of threads
    Asynchronous !Word
  deriving (Show, Read, Eq, Generic)

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

instance HasParser Threads where
  settingsParser =
    choice
      [ ( \case
            1 -> Synchronous
            w -> Asynchronous w
        )
          <$> setting
            [ help "How many threads to use to execute tests in asynchrnously",
              reader auto,
              option,
              long "jobs",
              long "threads",
              env "JOBS",
              env "THREADS",
              metavar "INT"
            ],
        setting
          [ help "Use only one thread, to execute tests synchronously",
            switch Synchronous,
            long "synchronous"
          ],
        Synchronous
          <$ setting
            [ help "Use only one thread, to execute tests synchronously",
              reader exists,
              env "SYNCHRONOUS",
              metavar "ANY"
            ],
        setting
          [ help "How parallel to run the test suite",
            confWith' "threads" $
              let f = \case
                    Nothing -> Just ByCapabilities
                    Just 1 -> Just Synchronous
                    Just n -> Just $ Asynchronous n
               in f <$> codec
          ]
      ]

data Iterations
  = -- | Run the test suite once, the default
    OneIteration
  | -- | Run the test suite for the given number of iterations, or until we can find flakiness
    Iterations !Word
  | -- | Run the test suite over and over, until we can find some flakiness
    Continuous
  deriving (Show, Read, Eq, Generic)

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

instance HasParser Iterations where
  settingsParser =
    choice
      [ setting
          [ help "Run the test suite over and over again until it fails, for example to diagnose flakiness",
            switch Continuous,
            long "continuous"
          ],
        ( \case
            0 -> Continuous
            1 -> OneIteration
            i -> Iterations i
        )
          <$> setting
            [ help "How many iterations of the suite to run, for example to diagnose flakiness",
              reader auto,
              option,
              long "iterations",
              metavar "INT"
            ],
        pure $ settingIterations defaultSettings
      ]

data ReportProgress
  = -- | Don't report any progress, the default
    ReportNoProgress
  | -- | Report progress
    ReportProgress
  deriving (Show, Read, Eq, Generic)

instance HasParser ReportProgress where
  settingsParser =
    choice
      [ setting
          [ help "Report per-example progress",
            switch ReportProgress,
            long "progress"
          ],
        setting
          [ help "Don't report per-example progress",
            switch ReportNoProgress,
            long "no-progress"
          ]
      ]
