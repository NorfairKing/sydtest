{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.ResourceFailureSpec (spec) where

import Control.Exception
import Data.Text (Text)
import Test.Syd
import Test.Syd.OptParse

spec :: Spec
spec = do
  insertResourceFailureSpec
  resourceFailureReportingSpec

resourceFailureReportingSpec :: Spec
resourceFailureReportingSpec = describe "a resource that fails after the tests inside it ran" $ do
  it "is reported, by the synchronous runner" $ do
    specForest <-
      execTestDefM defaultSettings $
        describe "database" $
          afterAll_ (throwIO (userError "could not drop the schema")) $
            it "reads a row" (pure () :: IO ())
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    case flattenResourceFailures (timedValue resultForest) of
      [(path, e)] -> do
        path `shouldBe` ["database", "afterAll"]
        displayException e `shouldContain` "could not drop the schema"
      other -> expectationFailure ("expected one resource failure, got " ++ show (length other))

  it "is reported, by the asynchronous runner" $ do
    -- The thread that catches it is not the one that builds the result forest
    -- here, so this is the half that goes by path.
    let settings = defaultSettings {settingThreads = Asynchronous 2}
    specForest <-
      execTestDefM settings $
        describe "database" $
          afterAll_ (throwIO (userError "could not drop the schema")) $
            it "reads a row" (pure () :: IO ())
    resultForest <- runSpecForestAsynchronously settings 2 specForest
    case flattenResourceFailures (timedValue resultForest) of
      [(path, e)] -> do
        path `shouldBe` ["database", "afterAll"]
        displayException e `shouldContain` "could not drop the schema"
      other -> expectationFailure ("expected one resource failure, got " ++ show (length other))

  it "leaves the results of the tests it was around" $ do
    specForest <-
      execTestDefM defaultSettings $
        afterAll_ (throwIO (userError "could not drop the schema")) $ do
          it "passes" (pure () :: IO ())
          it "fails" (expectationFailure "intentional" :: IO ())
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
    testSuiteStatSuccesses stats `shouldBe` 1
    testSuiteStatFailures stats `shouldBe` 1

  it "is in no count, and fails the suite anyway" $ do
    specForest <-
      execTestDefM defaultSettings $
        afterAll_ (throwIO (userError "could not drop the schema")) $
          it "passes" (pure () :: IO ())
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
    -- No count moves, because there is no test whose fault it is.
    testSuiteStatSuccesses stats `shouldBe` 1
    testSuiteStatFailures stats `shouldBe` 0
    -- The exit code is the only place it tells.
    shouldExitFail defaultSettings (timedValue resultForest) `shouldBe` True

  it "is reported for a resource that wraps the tests, too" $ do
    specForest <-
      execTestDefM defaultSettings $
        aroundAll_ (\inner -> inner >> throwIO (userError "could not close the pool")) $
          it "passes" (pure () :: IO ())
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    case flattenResourceFailures (timedValue resultForest) of
      [(path, e)] -> do
        path `shouldBe` ["aroundAll_"]
        displayException e `shouldContain` "could not close the pool"
      other -> expectationFailure ("expected one resource failure, got " ++ show (length other))

  it "is reported for a resource that never runs the tests inside it" $ do
    -- Waiting for results that are not coming is what this used to do.
    specForest <-
      execTestDefM defaultSettings $
        aroundAll_ (\_ -> pure ()) $
          it "never runs" (pure () :: IO ())
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    case flattenResourceFailures (timedValue resultForest) of
      [(path, e)] -> do
        path `shouldBe` ["aroundAll_"]
        displayException e `shouldContain` "without ever running the tests inside it"
      other -> expectationFailure ("expected one resource failure, got " ++ show (length other))

  it "still lets the tests run when the resource is fine" $ do
    specForest <-
      execTestDefM defaultSettings $
        afterAll_ (pure ()) $
          it "passes" (pure () :: IO ())
    resultForest <- runSpecForestSynchronously defaultSettings specForest
    let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
    testSuiteStatSuccesses stats `shouldBe` 1
    flattenResourceFailures (timedValue resultForest) `shouldSatisfy` null
    shouldExitFail defaultSettings (timedValue resultForest) `shouldBe` False

insertResourceFailureSpec :: Spec
insertResourceFailureSpec = describe "insertResourceFailure" $ do
  let failure = toException (userError "the resource failed")
      resourceFailurePaths :: ResultForest -> [[Text]]
      resourceFailurePaths = map fst . flattenResourceFailures

  it "puts a failure with no path at the end of the forest" $
    resourceFailurePaths (insertResourceFailure [] "afterAll" failure [])
      `shouldBe` [["afterAll"]]

  it "puts a failure under the group it happened below" $ do
    let forest = [DescribeNode "database" [], DescribeNode "cache" []]
    resourceFailurePaths (insertResourceFailure ["cache"] "afterAll" failure forest)
      `shouldBe` [["cache", "afterAll"]]

  it "puts a failure under nested groups" $ do
    let forest = [DescribeNode "database" [DescribeNode "schema" []]]
    resourceFailurePaths (insertResourceFailure ["database", "schema"] "afterAll" failure forest)
      `shouldBe` [["database", "schema", "afterAll"]]

  it "keeps a failure whose path is not in the forest, at the end" $ do
    -- Dropping it would be worse: the exception says what happened either way.
    let forest = [DescribeNode "database" []]
    resourceFailurePaths (insertResourceFailure ["cache"] "afterAll" failure forest)
      `shouldBe` [["afterAll"]]

  it "gives it to the first of two groups that share a path" $ do
    let forest = [DescribeNode "database" [], DescribeNode "database" []]
    resourceFailurePaths (insertResourceFailure ["database"] "afterAll" failure forest)
      `shouldBe` [["database", "afterAll"]]
