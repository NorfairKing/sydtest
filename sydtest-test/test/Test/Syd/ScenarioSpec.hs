{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.ScenarioSpec (spec) where

import Path
import Path.IO
import Test.Syd
import Test.Syd.OptParse

spec :: Spec
spec = do
  scenarioDir "test_resources/even" $ \rf ->
    it "contains an even number" $ do
      s <- readFile rf
      n <- readIO s
      (n :: Int) `shouldSatisfy` even
  scenarioDirRecur "test_resources/odd" $ \rf ->
    it "contains an odd number" $ do
      s <- readFile rf
      n <- readIO s
      (n :: Int) `shouldSatisfy` odd

  describe "scenarioDir" $ do
    it "defines a failing test when the directory is empty" $
      withSystemTempDir "sydtest-scenario" $ \tdir -> do
        specForest <-
          execTestDefM defaultSettings $
            scenarioDir (fromAbsDir tdir) $ \fp ->
              it "is never defined" $ fp `shouldBe` fp
        resultForest <- runSpecForestSynchronously defaultSettings specForest
        let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
        testSuiteStatFailures stats `shouldBe` 1
        testSuiteStatSuccesses stats `shouldBe` 0

    it "defines a failing test when the directory does not exist" $
      withSystemTempDir "sydtest-scenario" $ \tdir -> do
        specForest <-
          execTestDefM defaultSettings $
            scenarioDir (fromAbsDir (tdir </> [reldir|nonexistent|])) $ \fp ->
              it "is never defined" $ fp `shouldBe` fp
        resultForest <- runSpecForestSynchronously defaultSettings specForest
        let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
        testSuiteStatFailures stats `shouldBe` 1
        testSuiteStatSuccesses stats `shouldBe` 0

  describe "scenarioDirRecur" $
    it "defines a failing test when the directory contains only empty directories" $
      withSystemTempDir "sydtest-scenario" $ \tdir -> do
        createDir (tdir </> [reldir|subdir|])
        specForest <-
          execTestDefM defaultSettings $
            scenarioDirRecur (fromAbsDir tdir) $ \fp ->
              it "is never defined" $ fp `shouldBe` fp
        resultForest <- runSpecForestSynchronously defaultSettings specForest
        let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
        testSuiteStatFailures stats `shouldBe` 1
        testSuiteStatSuccesses stats `shouldBe` 0
