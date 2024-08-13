{-# LANGUAGE NumericUnderscores #-}

module Test.Syd.ExceptionSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Data.Time.Clock
import Test.Syd

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
