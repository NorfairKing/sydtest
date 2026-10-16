{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.FootgunSpec (spec) where

import Data.IORef
import System.Timeout (timeout)
import Test.Syd
import Test.Syd.OptParse

-- | A resource that misbehaves is still a resource, and the suite has to say
-- so rather than wait.
--
-- Every one of these used to leave a runner waiting for a result that was
-- never coming, which is a test suite that hangs with no idea why.  The
-- timeouts are what tell such a wait apart from a slow test.
spec :: Spec
spec = describe "a resource that does not run the tests inside it exactly once" $ do
  it "reports a resource that never runs the test" $ do
    specForest <-
      execTestDefM defaultSettings $
        around_ (\_ -> pure ()) $
          it "never runs" True
    mResultForest <- timeout 10_000_000 $ runSpecForestSynchronously defaultSettings specForest
    case mResultForest of
      Nothing -> expectationFailure "the runner waited for a test that was never run"
      Just resultForest -> do
        let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
        testSuiteStatFailures stats `shouldBe` 1
        testSuiteStatSuccesses stats `shouldBe` 0

  it "reports a resource around a whole group that never runs its tests" $ do
    specForest <-
      execTestDefM defaultSettings $
        aroundAll_ (\_ -> pure ()) $
          it "never runs" True
    mResultForest <- timeout 10_000_000 $ runSpecForestSynchronously defaultSettings specForest
    case mResultForest of
      Nothing -> expectationFailure "the runner waited for a group of tests that was never run"
      Just resultForest ->
        map fst (flattenResourceFailures (timedValue resultForest)) `shouldBe` [["aroundAll_"]]

  it "keeps the first run when a resource runs the test twice" $ do
    runsVar <- newIORef (0 :: Int)
    specForest <-
      execTestDefM defaultSettings $
        around_ (\inner -> inner >> inner) $
          it "runs twice" (modifyIORef' runsVar succ :: IO ())
    mResultForest <- timeout 10_000_000 $ runSpecForestSynchronously defaultSettings specForest
    case mResultForest of
      Nothing -> expectationFailure "the runner waited rather than keeping the first result"
      Just resultForest -> do
        let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
        testSuiteStatSuccesses stats `shouldBe` 1
        -- The wrapper did what it said, which is the part sydtest does not
        -- get to decide.  One result is reported for the one test.
        runs <- readIORef runsVar
        runs `shouldBe` 2

  it "keeps the first run when a resource runs a whole group twice" $ do
    runsVar <- newIORef (0 :: Int)
    specForest <-
      execTestDefM defaultSettings $
        aroundAll_ (\inner -> inner >> inner) $
          it "runs twice" (modifyIORef' runsVar succ :: IO ())
    mResultForest <- timeout 10_000_000 $ runSpecForestSynchronously defaultSettings specForest
    case mResultForest of
      Nothing -> expectationFailure "the runner waited rather than keeping the first results"
      Just resultForest -> do
        let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
        testSuiteStatSuccesses stats `shouldBe` 1
        runs <- readIORef runsVar
        runs `shouldBe` 2

  it "reports a resource that never runs the test, in the asynchronous runner too" $ do
    let settings = defaultSettings {settingThreads = Asynchronous 2}
    specForest <-
      execTestDefM settings $
        around_ (\_ -> pure ()) $
          it "never runs" True
    mResultForest <- timeout 10_000_000 $ runSpecForestAsynchronously settings 2 specForest
    case mResultForest of
      Nothing -> expectationFailure "the runner waited for a test that was never run"
      Just resultForest -> do
        let stats = computeTestSuiteStats settings (timedValue resultForest)
        testSuiteStatFailures stats `shouldBe` 1
