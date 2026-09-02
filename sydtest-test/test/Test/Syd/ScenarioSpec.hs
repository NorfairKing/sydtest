{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.ScenarioSpec (spec) where

import Path
import Path.IO
import Test.Syd
import Test.Syd.OptParse

spec :: Spec
spec = do
  let evens = [reldir|test_resources/even|]
  scenarioDir evens $ \rf ->
    it "contains an even number" $ do
      s <- readFile (fromRelFile (evens </> rf))
      n <- readIO s
      (n :: Int) `shouldSatisfy` even
  let odds = [reldir|test_resources/odd|]
  scenarioDirRecur odds $ \rf ->
    it "contains an odd number" $ do
      s <- readFile (fromRelFile (odds </> rf))
      n <- readIO s
      (n :: Int) `shouldSatisfy` odd
  let sames = [reldir|test_resources/same|]
  scenarioDirOfDirs sames $ \rd ->
    it "contains two files with the same contents" $ do
      a <- readFile (fromRelFile (sames </> rd </> [relfile|a|]))
      b <- readFile (fromRelFile (sames </> rd </> [relfile|b|]))
      a `shouldBe` b

  describe "scenarioDir" $ do
    it "defines a failing test when the directory is empty" $
      withSystemTempDir "sydtest-scenario" $ \tdir -> do
        specForest <-
          execTestDefM defaultSettings $
            scenarioDir tdir $ \fp ->
              it "is never defined" $ fp `shouldBe` fp
        resultForest <- runSpecForestSynchronously defaultSettings specForest
        let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
        testSuiteStatFailures stats `shouldBe` 1
        testSuiteStatSuccesses stats `shouldBe` 0

    it "defines a failing test when the directory does not exist" $
      withSystemTempDir "sydtest-scenario" $ \tdir -> do
        specForest <-
          execTestDefM defaultSettings $
            scenarioDir (tdir </> [reldir|nonexistent|]) $ \fp ->
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
            scenarioDirRecur tdir $ \fp ->
              it "is never defined" $ fp `shouldBe` fp
        resultForest <- runSpecForestSynchronously defaultSettings specForest
        let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
        testSuiteStatFailures stats `shouldBe` 1
        testSuiteStatSuccesses stats `shouldBe` 0

  describe "scenarioDirOfDirs" $ do
    it "defines a test for each subdirectory, ignoring files and deeper nesting" $
      withSystemTempDir "sydtest-scenario" $ \tdir -> do
        createDir (tdir </> [reldir|scenario|])
        createDir (tdir </> [reldir|scenario/nested|])
        writeFile (fromAbsFile (tdir </> [relfile|loose-file|])) ""
        specForest <-
          execTestDefM defaultSettings $
            scenarioDirOfDirs tdir $ \fp ->
              it "is the scenario directory, relative to the one given" $
                fp `shouldBe` [reldir|scenario|]
        resultForest <- runSpecForestSynchronously defaultSettings specForest
        let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
        testSuiteStatSuccesses stats `shouldBe` 1
        testSuiteStatFailures stats `shouldBe` 0

    it "defines a failing test when the directory contains no subdirectories" $
      withSystemTempDir "sydtest-scenario" $ \tdir -> do
        writeFile (fromAbsFile (tdir </> [relfile|loose-file|])) ""
        specForest <-
          execTestDefM defaultSettings $
            scenarioDirOfDirs tdir $ \fp ->
              it "is never defined" $ fp `shouldBe` fp
        resultForest <- runSpecForestSynchronously defaultSettings specForest
        let stats = computeTestSuiteStats defaultSettings (timedValue resultForest)
        testSuiteStatFailures stats `shouldBe` 1
        testSuiteStatSuccesses stats `shouldBe` 0
