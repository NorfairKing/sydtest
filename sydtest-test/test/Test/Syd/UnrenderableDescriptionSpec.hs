{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.UnrenderableDescriptionSpec (spec) where

import Control.Exception
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Test.Syd
import Test.Syd.OptParse

spec :: Spec
spec = describe "a description that cannot be rendered" $ do
  it "makes the test it names fail" $ do
    specForest <-
      execTestDefM defaultSettings $
        it (error "this test name throws") True
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
    testSuiteStatFailures stats `shouldBe` 1
    testSuiteStatSuccesses stats `shouldBe` 0

  it "makes a pending test fail, reason included" $ do
    specForest <-
      execTestDefM defaultSettings $
        pendingWith "is pending" (error "this pending reason throws")
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
    testSuiteStatFailures stats `shouldBe` 1
    testSuiteStatPending stats `shouldBe` 0

  it "becomes one failure for a describe, in place of its tests" $ do
    specForest <-
      execTestDefM defaultSettings $
        describe (error "this describe description throws") $ do
          it "is one test under it" True
          it "is another test under it" True
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
    testSuiteStatFailures stats `shouldBe` 1
    testSuiteStatSuccesses stats `shouldBe` 0

  it "leaves the rest of the suite to run and report" $ do
    specForest <-
      execTestDefM defaultSettings $ do
        describe (error "this describe description throws") $ it "is under it" True
        it "is a test that can be named" True
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
    testSuiteStatSuccesses stats `shouldBe` 1
    testSuiteStatFailures stats `shouldBe` 1

  it "is decided before any test runs, so both output formats agree" $ do
    -- The terse output never renders a description, so until these were forced
    -- up front, the same suite passed piped into a file and died in a
    -- terminal.  Both runners get their forest from the same place now, which
    -- is what makes the two agree.
    let settings = defaultSettings {settingThreads = Asynchronous 2}
    specForest <-
      execTestDefM settings $
        it (error "this test name throws") True
    resultForest <- runSpecForestAsynchronously settings 2 specForest
    let stats = computeTestSuiteStats settings (timedValue resultForest)
    testSuiteStatFailures stats `shouldBe` 1
    testSuiteStatSuccesses stats `shouldBe` 0

  it "does not run the tests under a describe it could not name" $ do
    -- The stats cannot tell "not run" from "run but not reported", so these
    -- tests say for themselves whether they ran.
    ranVar <- newIORef (0 :: Int)
    specForest <-
      execTestDefM defaultSettings $
        describe (error "this describe description throws") $
          it "counts itself as having run" (modifyIORef' ranVar succ :: IO ())
    _ <- runSpecForestSynchronously defaultSettings specForest
    ran <- readIORef ranVar
    ran `shouldBe` 0

  it "says what went wrong" $ do
    specForest <-
      execTestDefM defaultSettings $
        it (error "this test name throws") True
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    case flattenSpecForest (timedValue resultForest) of
      [(path, td)] -> do
        path `shouldBe` ["<description could not be rendered>"]
        -- One result, because the same description throws the same way every
        -- time and the stand-in test is not retried.
        case testRunReportRawResults (timedValue (testDefVal td)) of
          result :| [] -> case testRunResultException result of
            Nothing -> expectationFailure "the failure did not carry the exception that the description threw"
            Just e -> displayException e `shouldContain` "this test name throws"
          results -> expectationFailure ("expected one result, got " ++ show (length results))
      other -> expectationFailure ("expected one test result, got " ++ show (length other))
