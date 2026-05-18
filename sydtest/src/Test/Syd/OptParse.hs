{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.OptParse where

import Autodocodec
import Control.Applicative
import Control.Concurrent (getNumCapabilities)
import Control.Monad
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import OptEnvConf
import Path
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
    -- | Whether to use colour in the output
    settingTerminalCapabilities :: !TerminalCapabilities,
    -- | The filters to use to select which tests to run
    settingFilters :: ![Text],
    -- | Whether to stop upon the first test failure
    settingFailFast :: !Bool,
    -- | How many iterations to use to look diagnose flakiness
    settingIterations :: !Iterations,
    -- | How many microseconds wait for a test to finish before considering it failed
    settingTimeout :: !Timeout,
    -- | How many times to retry a test for flakiness diagnostics
    settingRetries :: !Word,
    -- | Whether to fail when any flakiness is detected in tests declared as flaky
    settingFailOnFlaky :: !Bool,
    -- | Whether to skip running tests that have already passed.
    settingSkipPassed :: !Bool,
    -- | Where to store the report
    settingReportFile :: !(Maybe (Path Abs File)),
    -- | How to report progress
    settingReportProgress :: !ReportProgress,
    -- | Profiling mode
    settingProfile :: !Bool,
    -- | Output format
    settingOutputFormat :: !OutputFormat,
    -- | When 'Just', run in mutation testing mode under the selected
    -- sub-mode.  'Nothing' means normal test execution.
    settingMutation :: !(Maybe MutationSettings)
  }
  deriving (Show, Eq, Generic)

-- | Top-level mutation-testing configuration.  The 'mutationMode' selects
-- which of the three child mutation entry points runs, and
-- 'mutationFailFast' is a cross-mode flag.  All other mutation-related
-- options live on the mode-specific records.
--
-- Parent-side orchestration (coverage collection, mutation dispatch) is
-- handled by the @sydtest-mutation-driver@ executable, which spawns
-- sydtest test suites as children using these child modes.
data MutationSettings = MutationSettings
  { -- | Stop the mutation/coverage run as soon as a surviving, uncovered,
    -- or failing test is observed.  True suits CI; set to false for
    -- iterated development where the full report is wanted.
    mutationFailFast :: !Bool,
    mutationMode :: !MutationMode
  }
  deriving (Show, Eq, Generic)

-- | One of the three child-side mutation entry points, with the options
-- it needs.  Parent-side modes live in @sydtest-mutation-driver@.
data MutationMode
  = -- | Child process: print every leaf test's id on stdout and exit.
    -- Used by the driver to enumerate tests for the coverage phase.
    MutationModeCoverageList
  | -- | Child process that collects coverage for one test.
    MutationModeCoverageChild !CoverageChildSettings
  | -- | Child process that runs only the tests covering one mutation.
    MutationModeMutateChild !MutationChildSettings
  deriving (Show, Eq, Generic)

-- | Options for the coverage-child process: the single test to run, plus
-- the two output files for its coverage map and wall-clock baseline.
data CoverageChildSettings = CoverageChildSettings
  { coverageChildTestId :: !Text,
    coverageChildOutput :: !FilePath,
    coverageChildBaselineOutput :: !FilePath,
    coverageChildSuiteName :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

-- | Options for the mutation-child process: the mutation id to evaluate,
-- the augmented-manifest directory to read it from, and the suite name
-- whose covering tests should be selected.
data MutationChildSettings = MutationChildSettings
  { mutationChildId :: !String,
    mutationChildAugmentedManifestDir :: !(Maybe (Path Abs Dir)),
    mutationChildSuiteName :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

-- | Output format for test results
data OutputFormat
  = -- | Pretty output with colors, unicode symbols, and detailed formatting
    OutputFormatPretty
  | -- | Terse output optimized for machine/AI consumption
    OutputFormatTerse
  deriving (Show, Eq, Generic, Enum, Bounded)

instance HasCodec OutputFormat where
  codec =
    stringConstCodec $
      (OutputFormatPretty, "pretty")
        :| [(OutputFormatTerse, "terse")]

instance HasParser Settings where
  settingsParser =
    subEnv_ "sydtest" $
      withConfigurableYamlConfig (runIO $ resolveFile' ".sydtest.yaml") $
        checkMapIO combine settingsParser
    where
      combine :: Flags -> IO (Either String Settings)
      combine Flags {..} = do
        let d :: forall a. (Settings -> a) -> a
            d func = func defaultSettings
        terminalCapabilities <- case flagColour of
          Just False -> pure WithoutColours
          Just True -> pure With8BitColours
          Nothing -> case flagAiExecutor of
            Just True -> pure WithoutColours
            _ -> detectTerminalCapabilities

        let threads =
              fromMaybe
                ( if flagDebug
                    then Synchronous
                    else d settingThreads
                )
                flagThreads
        case threads of
          ByCapabilities -> do
            i <- getNumCapabilities

            when (i == 1) $ do
              let outputLine :: [Chunk] -> IO ()
                  outputLine lineChunks = liftIO $ do
                    putChunksLocaleWith terminalCapabilities lineChunks
                    TIO.putStrLn ""
              mapM_
                ( outputLine
                    . (: [])
                    . fore red
                )
                [ chunk "WARNING: Only one CPU core detected, make sure to compile your test suite with these ghc options:",
                  chunk "         -threaded -rtsopts -with-rtsopts=-N",
                  chunk "         (This is important for correctness as well as speed, as a parallel test suite can find thread safety problems.)"
                ]
          _ -> pure ()
        errOrProgress <- case flagReportProgress of
          Nothing ->
            pure $
              Right $
                if threads == Synchronous
                  then
                    if flagDebug
                      then ReportProgress
                      else d settingReportProgress
                  else d settingReportProgress
          Just ReportNoProgress -> pure $ Right ReportNoProgress
          Just ReportProgress ->
            if threads /= Synchronous
              then pure $ Left "Reporting progress in asynchronous runners is not supported. You can use --synchronous or --debug to use a synchronous runner."
              else pure $ Right ReportProgress
        let combined = do
              progress <- errOrProgress
              mMutation <- resolveMutationSettings Flags {..}
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
                    settingTerminalCapabilities = terminalCapabilities,
                    settingFilters = flagFilters,
                    settingFailFast =
                      fromMaybe
                        ( if flagDebug
                            then True
                            else d settingFailFast
                        )
                        flagFailFast,
                    settingIterations = flagIterations,
                    settingTimeout = flagTimeout,
                    settingRetries =
                      fromMaybe
                        ( if flagDebug
                            then 0
                            else d settingRetries
                        )
                        flagRetries,
                    settingFailOnFlaky = flagFailOnFlaky,
                    settingSkipPassed = flagSkipPassed,
                    settingReportFile = flagReportFile,
                    settingReportProgress = progress,
                    settingProfile = flagProfile,
                    settingOutputFormat =
                      fromMaybe
                        ( case flagAiExecutor of
                            Nothing -> OutputFormatPretty
                            Just False -> OutputFormatPretty
                            Just True -> OutputFormatTerse
                        )
                        flagOutputFormat,
                    settingMutation = mMutation
                  }
        pure combined

-- | Pick at most one child-side 'MutationMode' from the parsed flags.
--
-- The three flags are dispatched in priority order:
--
-- 1. 'flagMutationCoverageList' selects coverage-list (enumerate tests).
-- 2. 'flagMutationCoverageOne'  selects coverage-child.
-- 3. 'flagMutationOne'          selects mutation-child.
--
-- Parent-side orchestration is the job of @sydtest-mutation-driver@.
resolveMutationSettings :: Flags -> Either String (Maybe MutationSettings)
resolveMutationSettings Flags {..} =
  let failFast = fromMaybe defaultMutationFailFast flagMutationFailFast
      mkMutation mode = Just MutationSettings {mutationFailFast = failFast, mutationMode = mode}
   in case (flagMutationCoverageList, flagMutationCoverageOne, flagMutationOne) of
        (True, _, _) ->
          pure $ mkMutation MutationModeCoverageList
        (False, Just tid, _) -> do
          outputFile <- case flagMutationCoverageOutput of
            Just f -> Right f
            Nothing -> Left "--mutation-coverage-one requires --mutation-coverage-output"
          baselineFile <- case flagMutationCoverageBaselineOutput of
            Just f -> Right f
            Nothing -> Left "--mutation-coverage-one requires --mutation-coverage-baseline-output"
          pure $
            mkMutation $
              MutationModeCoverageChild
                CoverageChildSettings
                  { coverageChildTestId = tid,
                    coverageChildOutput = outputFile,
                    coverageChildBaselineOutput = baselineFile,
                    coverageChildSuiteName = flagMutationSuiteName
                  }
        (False, Nothing, Just mid) ->
          pure $
            mkMutation $
              MutationModeMutateChild
                MutationChildSettings
                  { mutationChildId = mid,
                    mutationChildAugmentedManifestDir = flagMutationAugmentedManifestDir,
                    mutationChildSuiteName = flagMutationSuiteName
                  }
        (False, Nothing, Nothing) -> pure Nothing

-- | Default value of 'mutationFailFast'.  True suits CI so a single
-- survivor aborts the run; flip to False locally for the full report.
defaultMutationFailFast :: Bool
defaultMutationFailFast = True

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
          settingTerminalCapabilities = With8BitColours,
          settingFilters = mempty,
          settingFailFast = False,
          settingIterations = OneIteration,
          settingTimeout = TimeoutAfterMicros defaultTimeout,
          settingRetries = defaultRetries,
          settingFailOnFlaky = False,
          settingSkipPassed = False,
          settingReportProgress = ReportNoProgress,
          settingReportFile = Nothing,
          settingProfile = False,
          settingOutputFormat = OutputFormatPretty,
          settingMutation = Nothing
        }

-- 60 seconds
defaultTimeout :: Int
defaultTimeout = 60_000_000

defaultRetries :: Word
defaultRetries = 3

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
    flagTimeout :: !Timeout,
    flagFailOnFlaky :: !Bool,
    flagSkipPassed :: !Bool,
    flagReportFile :: !(Maybe (Path Abs File)),
    flagReportProgress :: !(Maybe ReportProgress),
    flagDebug :: !Bool,
    flagProfile :: !Bool,
    flagAiExecutor :: !(Maybe Bool),
    flagOutputFormat :: !(Maybe OutputFormat),
    flagMutationAugmentedManifestDir :: !(Maybe (Path Abs Dir)),
    flagMutationOne :: !(Maybe String),
    flagMutationSuiteName :: !(Maybe Text),
    flagMutationCoverageOne :: !(Maybe Text),
    flagMutationCoverageOutput :: !(Maybe FilePath),
    flagMutationCoverageBaselineOutput :: !(Maybe FilePath),
    flagMutationCoverageList :: !Bool,
    flagMutationFailFast :: !(Maybe Bool)
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
    flagTimeout <- settingsParser
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
    flagSkipPassed <-
      yesNoSwitch
        [ help $
            unlines
              [ "Skip tests that have already passed. When every test has passed, rerun them all.",
                "Note that you have to run with this flag once before it can activate."
              ],
          name "skip-passed",
          value $ settingSkipPassed defaultSettings
        ]
    flagReportFile <-
      optional $
        filePathSetting
          [ help "Where to store the the test report for --skip-passed",
            name "report-file"
          ]
    flagReportProgress <- optional settingsParser
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
    flagAiExecutor <-
      optional $
        choice
          [ setting
              [ help "Indicate that an AI is executing tests, sets defaults to 'no colours' and 'terse output'",
                switch True,
                long "ai-executor"
              ],
            setting
              [ help "Turn off ai mode. This lets AIs opt out of ai-executor mode",
                switch False,
                long "no-ai-executor"
              ],
            setting
              [ help "Activate AI executor mode based on env vars",
                reader exists,
                -- Feel free to add env vars here.
                unprefixedEnv "CLAUDECODE",
                metavar "ANY"
              ]
          ]

    flagOutputFormat <-
      optional $
        choice
          [ setting
              [ help "Use terse output (compact, no colors, failures only)",
                switch OutputFormatTerse,
                long "terse"
              ],
            setting
              [ help "Use pretty output (colors, unicode, detailed formatting)",
                switch OutputFormatPretty,
                long "pretty"
              ]
          ]
    flagMutationAugmentedManifestDir <-
      optional $
        directoryPathSetting
          [ help "Directory for manifest-augmented.json; defaults to current working directory",
            option,
            long "mutation-augmented-manifest-dir"
          ]
    flagMutationOne <-
      optional $
        setting
          [ help "Run only this single mutation id (used internally by child processes)",
            reader str,
            option,
            long "mutation-one",
            metavar "MUTATION_ID",
            hidden
          ]
    flagMutationSuiteName <-
      optional $
        setting
          [ help "Name of this test suite executable (used in multi-suite mutation testing)",
            reader str,
            option,
            long "mutation-suite-name",
            metavar "NAME",
            hidden
          ]
    flagMutationCoverageOne <-
      optional $
        setting
          [ help "Collect coverage for only this single test id (used internally by coverage child processes)",
            reader str,
            option,
            long "mutation-coverage-one",
            metavar "TEST_ID",
            hidden
          ]
    flagMutationCoverageOutput <-
      optional $
        setting
          [ help "File path where coverage child process writes its coverage map (used internally)",
            reader str,
            option,
            long "mutation-coverage-output",
            metavar "FILE",
            hidden
          ]
    flagMutationCoverageBaselineOutput <-
      optional $
        setting
          [ help "File path where coverage child process writes its baseline timing (used internally)",
            reader str,
            option,
            long "mutation-coverage-baseline-output",
            metavar "FILE",
            hidden
          ]
    flagMutationCoverageList <-
      setting
        [ help "List every leaf test id on stdout and exit (used internally by the mutation driver to enumerate tests)",
          switch True,
          long "mutation-coverage-list",
          hidden,
          value False
        ]
    flagMutationFailFast <-
      optional $
        yesNoSwitch
          [ help "Stop the mutation run as soon as a surviving or uncovered mutation is observed",
            name "mutation-fail-fast"
          ]
    pure Flags {..}

data Timeout
  = DoNotTimeout
  | TimeoutAfterMicros !Int
  deriving (Show, Read, Eq, Generic)

instance HasCodec Timeout where
  codec = dimapCodec f g codec
    where
      f = \case
        Nothing -> DoNotTimeout
        Just i -> TimeoutAfterMicros i
      g = \case
        DoNotTimeout -> Nothing
        TimeoutAfterMicros i -> Just i

instance HasParser Timeout where
  settingsParser =
    choice
      [ setting
          [ help "Don't timeout",
            switch DoNotTimeout,
            long "no-timeout"
          ],
        TimeoutAfterMicros
          <$> setting
            [ help "After how many microseconds to consider a test failed",
              reader auto,
              name "timeout",
              value defaultTimeout,
              metavar "MICROSECONDS"
            ]
      ]

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
            [ help "How many threads to use to execute tests in asynchronously",
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
