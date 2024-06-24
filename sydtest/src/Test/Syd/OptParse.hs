{-# LANGUAGE ApplicativeDo #-}
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
import Data.Functor ((<&>))
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Env
import GHC.Generics (Generic)
import OptEnvConf
import Path
import Path.IO
import Paths_sydtest (version)
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
getSettings = runSettingsParser version

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

instance HasParser Settings where
  settingsParser = do
    settingSeed <-
      setting
        [ help "The seed to use for deterministic randomness",
          name "seed"
        ]
    settingRandomiseExecutionOrder <-
      setting
        [ help "Randomise the execution order of the tests in the test suite",
          name "randomise-execution-order",
          name "randomize-execution-order"
        ]
    settingThreads <-
      setting
        [ help "How parallel to run the test suite",
          name "parallel"
        ]
    settingMaxSuccess <-
      setting
        [ help "How many examples to run a property test with",
          name "max-success"
        ]
    settingMaxSize <-
      setting
        [ help "The maximum size parameter to supply to generators",
          name "max-size"
        ]
    settingMaxDiscard <-
      setting
        [ help "The maximum number of discarded examples per tested example",
          name "max-discard"
        ]
    settingMaxShrinks <-
      setting
        [ help "The maximum number of tries to use while shrinking a counterexample.",
          name "max-shrinks"
        ]
    settingGoldenStart <-
      setting
        [ help "Whether to write golden tests if they do not exist yet",
          name "golden-start"
        ]
    settingGoldenReset <-
      setting
        [ help "Whether to overwrite golden tests instead of having them fail",
          name "golden-reset"
        ]
    settingColour <-
      setting
        [ help "Whether to use colour in the output",
          name "colour",
          name "color"
        ]
    settingFilters <-
      many $
        setting
          [ help "The filters to use to select which tests to run",
            reader str,
            argument
          ]
    settingFailFast <-
      setting
        [ help "Whether to stop upon the first test failure",
          name "fail-fast"
        ]
    settingIterations <-
      setting
        [ help "How many iterations to use to look diagnose flakiness",
          name "iterations"
        ]
    settingRetries <-
      setting
        [ help "How many times to retry a test for flakiness diagnostics",
          name "retries"
        ]
    settingFailOnFlaky <-
      setting
        [ help "Whether to fail when any flakiness is detected in tests declared as flaky",
          name "fail-on-flaky"
        ]
    settingReportProgress <-
      setting
        [ help "How to report progress",
          reader auto,
          name "progress"
        ]
    settingDebug <-
      setting
        [ help "Debug mode",
          name "debug"
        ]
    settingProfile <-
      setting
        [ help "Profiling mode",
          name "profiling"
        ]

    pure Settings {..}

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
