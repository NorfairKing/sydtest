{-# LANGUAGE NumericUnderscores #-}

module Test.Syd.ExceptionSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Data.Time.Clock
import System.Timeout (timeout)
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

  it "reports a test that throws an asynchronously-classified exception as a failure" $ do
    specForest <-
      execTestDefM defaultSettings $
        it "throws an asynchronously-classified exception" (throwIO SynchronousButClassifiedAsynchronous :: IO ())
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
    testSuiteStatFailures stats `shouldBe` 1
    testSuiteStatSuccesses stats `shouldBe` 0

  it "lets expectFailing accept a test that throws an asynchronously-classified exception" $ do
    specForest <-
      execTestDefM defaultSettings $
        expectFailing $
          it "throws an asynchronously-classified exception" (throwIO SynchronousButClassifiedAsynchronous :: IO ())
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
    testSuiteStatSuccesses stats `shouldBe` 1
    testSuiteStatFailures stats `shouldBe` 0

  it "reports an asynchronously-classified exception as a failure in the asynchronous runner too" $ do
    let settings = defaultSettings {settingThreads = Asynchronous 2}
    specForest <-
      execTestDefM settings $
        it "throws an asynchronously-classified exception" (throwIO SynchronousButClassifiedAsynchronous :: IO ())
    -- The timeout is here so that the asynchronous runner waiting forever for
    -- a result that the test will never produce shows up as a failure instead
    -- of a stuck test suite.
    mResultForest <- timeout 10_000_000 $ runSpecForestAsynchronously settings 2 specForest
    case mResultForest of
      Nothing -> expectationFailure "the asynchronous runner never finished"
      Just resultForest -> do
        let stats = computeTestSuiteStats settings (timedValue resultForest)
        testSuiteStatFailures stats `shouldBe` 1
        testSuiteStatSuccesses stats `shouldBe` 0
