{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.ExceptionSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad (void)
import Control.Monad.Reader (ReaderT)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock
import GHC.Stack (emptyCallStack)
import System.Timeout (timeout)
import Test.QuickCheck (ioProperty, property)
import Test.Syd
import Test.Syd.OptParse

-- | An exception that a test throws itself, but that is classified as
-- asynchronous.
--
-- This is not exotic: @effectful@'s @Error@ effect is defined this way, so
-- every test that fails through it throws one of these.
-- See https://github.com/NorfairKing/sydtest/issues/110
data SynchronousButClassifiedAsynchronous = SynchronousButClassifiedAsynchronous
  deriving (Show, Eq)

instance Exception SynchronousButClassifiedAsynchronous where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

spec :: TestDefM outers () ()
spec = describe "exception handling" $ do
  it "stops immediatly with async" $ do
    -- Tests that when an async exception is sent to sydTest (for example,
    -- ctrl-c), it behaves as expected and terminates immediately and does not
    -- do anything surprising.
    startTime <- liftIO getCurrentTime

    -- Runs two threads, one will be done in 1s, and the other is a test suite
    -- with one test which should run for 10s.
    -- When the first threads terminate, it will throw AsyncCancel in the
    -- sydTest test suite, which should terminate asap.
    -- This will be checked using the timer t. That's fragile, but we know that:
    --
    -- t should be more than 100ms (because of the first threadDelay)
    -- t should be no much more than 100ms. Especially, it should not be 10s
    -- (waiting for the complete threadDelay in the test suite) or even more if
    -- the exception is completly ignored (or test is retried)
    _ <- race (threadDelay 100_000) $ do
      sydTest $ do
        it "is a dumb slow test" $ do
          threadDelay 10_000_000

    endTime <- liftIO getCurrentTime

    -- Less than 1 second
    (endTime `diffUTCTime` startTime) `shouldSatisfy` (< 1)
    pure ()

  it "lets a Ctrl-C's UserInterrupt stop the suite" $ do
    -- What the RTS delivers on Ctrl-C is 'UserInterrupt', to the thread
    -- running the suite.
    -- It must come back out of the runner rather than fail the test it lands in.
    let settings = defaultSettings {settingThreads = Asynchronous 2}
    specForest <-
      execTestDefM settings $
        it "sleeps for far longer than the interrupt takes to arrive" (threadDelay 10_000_000 :: IO ())
    startTime <- liftIO getCurrentTime
    runningSuite <- async $ runSpecForestAsynchronously settings 2 specForest
    liftIO $ threadDelay 100_000
    liftIO $ throwTo (asyncThreadId runningSuite) UserInterrupt
    result <- waitCatch runningSuite
    endTime <- liftIO getCurrentTime
    case result of
      Right _ -> expectationFailure "the interrupt was swallowed: the runner produced a result forest"
      Left e -> do
        fromException e `shouldBe` Just UserInterrupt
        -- Not the ten seconds it would take if the interrupt were caught as
        -- the sleeping test's failure and the suite carried on, with room to
        -- spare for a machine that is busy building the rest of this repo.
        (endTime `diffUTCTime` startTime) `shouldSatisfy` (< 5)

  describe "a test that throws an asynchronously-classified exception fails with that exception" $ do
    let settings = defaultSettings {settingRetries = 1}

    it "Bool" $ do
      specForest <-
        execTestDefM settings $
          it "throws" (throw SynchronousButClassifiedAsynchronous :: Bool)
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "arg -> Bool" $ do
      specForest <-
        execTestDefM settings $
          before (pure ()) $
            it "throws" (\() -> throw SynchronousButClassifiedAsynchronous :: Bool)
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> innerArg -> Bool" $ do
      specForest <-
        execTestDefM settings $
          beforeAll (pure ()) $
            before (pure ()) $
              itWithBoth "throws" (\() () -> throw SynchronousButClassifiedAsynchronous :: Bool)
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "IO ()" $ do
      specForest <-
        execTestDefM settings $
          it "throws" (throwIO SynchronousButClassifiedAsynchronous :: IO ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "arg -> IO ()" $ do
      specForest <-
        execTestDefM settings $
          before (pure ()) $
            it "throws" (\() -> throwIO SynchronousButClassifiedAsynchronous :: IO ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> innerArg -> IO ()" $ do
      specForest <-
        execTestDefM settings $
          beforeAll (pure ()) $
            before (pure ()) $
              itWithBoth "throws" (\() () -> throwIO SynchronousButClassifiedAsynchronous :: IO ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "ReaderT env IO ()" $ do
      specForest <-
        execTestDefM settings $
          before (pure ()) $
            it "throws" (liftIO (throwIO SynchronousButClassifiedAsynchronous) :: ReaderT () IO ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> ReaderT env IO ()" $ do
      specForest <-
        execTestDefM settings $
          beforeAll (pure ()) $
            before (pure ()) $
              itWithBoth "throws" (\() -> liftIO (throwIO SynchronousButClassifiedAsynchronous) :: ReaderT () IO ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "Property" $ do
      specForest <-
        execTestDefM settings $
          it "throws" (ioProperty (throwIO SynchronousButClassifiedAsynchronous :: IO Bool))
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "arg -> Property" $ do
      specForest <-
        execTestDefM settings $
          before (pure ()) $
            it "throws" (\() -> ioProperty (throwIO SynchronousButClassifiedAsynchronous :: IO Bool))
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> innerArg -> Property" $ do
      specForest <-
        execTestDefM settings $
          beforeAll (pure ()) $
            before (pure ()) $
              itWithBoth "throws" (\() () -> ioProperty (throwIO SynchronousButClassifiedAsynchronous :: IO Bool))
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "Property, when its around wrapper throws" $ do
      specForest <-
        execTestDefM settings $
          around (\_ -> throwIO SynchronousButClassifiedAsynchronous) $
            it "throws" (\() -> property True)
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "GoldenTest a" $ do
      specForest <-
        execTestDefM settings $
          it
            "throws"
            ( GoldenTest
                { goldenTestRead = pure (Just ()),
                  goldenTestProduce = throwIO SynchronousButClassifiedAsynchronous,
                  goldenTestWrite = \() -> pure (),
                  goldenTestCompare = \() () -> pure Nothing
                }
            )
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "arg -> GoldenTest a" $ do
      specForest <-
        execTestDefM settings $
          before (pure ()) $
            it
              "throws"
              ( \() ->
                  GoldenTest
                    { goldenTestRead = pure (Just ()),
                      goldenTestProduce = throwIO SynchronousButClassifiedAsynchronous,
                      goldenTestWrite = \() -> pure (),
                      goldenTestCompare = \() () -> pure Nothing
                    }
              )
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> innerArg -> GoldenTest a" $ do
      specForest <-
        execTestDefM settings $
          beforeAll (pure ()) $
            before (pure ()) $
              itWithBoth
                "throws"
                ( \() () ->
                    GoldenTest
                      { goldenTestRead = pure (Just ()),
                        goldenTestProduce = throwIO SynchronousButClassifiedAsynchronous,
                        goldenTestWrite = \() -> pure (),
                        goldenTestCompare = \() () -> pure Nothing
                      }
                )
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "IO (GoldenTest a)" $ do
      specForest <-
        execTestDefM settings $
          it "throws" (throwIO SynchronousButClassifiedAsynchronous :: IO (GoldenTest ()))
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "arg -> IO (GoldenTest a)" $ do
      specForest <-
        execTestDefM settings $
          before (pure ()) $
            it "throws" (\() -> throwIO SynchronousButClassifiedAsynchronous :: IO (GoldenTest ()))
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> innerArg -> IO (GoldenTest a)" $ do
      specForest <-
        execTestDefM settings $
          beforeAll (pure ()) $
            before (pure ()) $
              itWithBoth "throws" (\() () -> throwIO SynchronousButClassifiedAsynchronous :: IO (GoldenTest ()))
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "StagedGolden a" $ do
      specForest <-
        execTestDefM settings $
          it "throws" (stagedGolden (\_ -> throwIO SynchronousButClassifiedAsynchronous) :: StagedGolden ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "arg -> StagedGolden a" $ do
      specForest <-
        execTestDefM settings $
          before (pure ()) $
            it "throws" (\() -> stagedGolden (\_ -> throwIO SynchronousButClassifiedAsynchronous) :: StagedGolden ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> innerArg -> StagedGolden a" $ do
      specForest <-
        execTestDefM settings $
          beforeAll (pure ()) $
            before (pure ()) $
              itWithBoth "throws" (\() () -> stagedGolden (\_ -> throwIO SynchronousButClassifiedAsynchronous) :: StagedGolden ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

  it "lets a staged golden test's ThreadKilled stop the suite, like any other test's" $ do
    let settings = defaultSettings {settingRetries = 1}
    specForest <-
      execTestDefM settings $
        it "throws" (stagedGolden (\_ -> throwIO ThreadKilled) :: StagedGolden ())
    result <- try $ runSpecForestSynchronously settings specForest
    either (Just . fromException) (const Nothing) result `shouldBe` Just (Just ThreadKilled)

  it "lets expectFailing accept a test that throws an asynchronously-classified exception" $ do
    specForest <-
      execTestDefM defaultSettings $
        expectFailing $
          it "throws an asynchronously-classified exception" (throwIO SynchronousButClassifiedAsynchronous :: IO ())
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    map
      (\(_, td) -> (testRunReportFailed defaultSettings (timedValue (testDefVal td)), NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) (testRunReportRawResults (timedValue (testDefVal td)))))
      (flattenSpecForest (timedValue resultForest))
      `shouldBe` [(False, (TestFailed, Just SynchronousButClassifiedAsynchronous) :| [])]

  it "reports an asynchronously-classified exception as a failure in the asynchronous runner too" $ do
    let settings =
          defaultSettings
            { settingThreads = Asynchronous 2,
              settingRetries = 1
            }
    specForest <-
      execTestDefM settings $
        it "throws an asynchronously-classified exception" (throwIO SynchronousButClassifiedAsynchronous :: IO ())
    -- The timeout is here so that the asynchronous runner waiting forever for
    -- a result that the test will never produce shows up as a failure instead
    -- of a stuck test suite.
    mResultForest <- timeout 10_000_000 $ runSpecForestAsynchronously settings 2 specForest
    case mResultForest of
      Nothing -> expectationFailure "the asynchronous runner never finished"
      Just resultForest ->
        map
          (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
          (flattenSpecForest (timedValue resultForest))
          `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

  it "lets async's cancellation stop a running suite" $ do
    -- 'race' cancels the losing side, here a suite in the middle of a test.
    -- Caught as that test's failure, the cancellation would let the suite carry
    -- on with the next test, and 'race' would wait for all of it.
    let settings = defaultSettings {settingRetries = 1}
    specForest <-
      execTestDefM settings $ do
        it "sleeps for far longer than the cancellation takes to arrive" (threadDelay 10_000_000 :: IO ())
        it "sleeps for far longer too" (threadDelay 10_000_000 :: IO ())
    result <- timeout 5_000_000 $ race (threadDelay 100_000) (runSpecForestSynchronously settings specForest)
    fmap void result `shouldBe` Just (Left ())

  it "still cuts a test short when the timeout is what interrupts it" $ do
    -- A timeout must get past the test to the timeout that threw it, rather
    -- than being caught as that test's failure.
    -- Catching it reports the test as failed with an exception, with no
    -- timeout anywhere, which is what this asserts against.
    let settings =
          defaultSettings
            { settingThreads = Asynchronous 2,
              settingTimeout = TimeoutAfterMicros 100_000
            }
    specForest <-
      execTestDefM settings $
        it "sleeps for far longer than the timeout" (threadDelay 10_000_000 :: IO ())
    mResultForest <- timeout 5_000_000 $ runSpecForestAsynchronously settings 2 specForest
    case mResultForest of
      Nothing -> expectationFailure "the timeout did not cut the test short"
      Just resultForest ->
        case flattenSpecForest (timedValue resultForest) of
          [(_, td)] -> case testRunReportRawResults (timedValue (testDefVal td)) of
            result :| [] -> testRunResultExtraInfo result `shouldBe` Just "Timeout!"
            results -> expectationFailure ("a timed-out test is not retried, so expected one result, got " ++ show (length results))
          other -> expectationFailure ("expected one test result, got " ++ show (length other))

  it "fails a test whose test function throws, in the synchronous runner" $ do
    let forest :: TestForest '[] ()
        forest =
          [ DefSpecifyNode
              "throws instead of producing a result"
              TDef
                { testDefVal = \_progressReporter _wrapper -> throwIO (userError "this test function throws"),
                  testDefCallStack = emptyCallStack
                }
              ()
          ]
    let settings = defaultSettings {settingRetries = 1}
    resultForest <- runSpecForestSynchronously settings forest
    map
      (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
      (flattenSpecForest (timedValue resultForest))
      `shouldBe` [(TestFailed, Just (userError "this test function throws")) :| []]
