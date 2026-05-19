{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.Mutation.Driver.SuitePkgSpec (spec) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import Path
import Path.IO (createDirIfMissing, withSystemTempDir)
import Test.Syd
import Test.Syd.Mutation.Driver.OptParse (SuiteConfig (..), SuitePkgSpec (..))
import Test.Syd.Mutation.Driver.SuitePkg (SuitePkgWalkError (..), walkSuitePkgs)

spec :: Spec
spec = describe "walkSuitePkgs" $ do
  it "expands one spec into one SuiteConfig per file under <root>/test" $
    withSystemTempDir "walk-one" $ \dir -> do
      let root = dir </> [reldir|pkg|]
          rd = dir </> [reldir|resource|]
          testDir = root </> [reldir|test|]
      createDirIfMissing True testDir
      createDirIfMissing True rd
      B.writeFile (fromAbsFile (testDir </> [relfile|first-test|])) ""
      B.writeFile (fromAbsFile (testDir </> [relfile|second-test|])) ""
      result <-
        walkSuitePkgs
          [ SuitePkgSpec
              { suitePkgSpecPname = "mypkg",
                suitePkgSpecBuiltTestPkgRoot = root,
                suitePkgSpecResourceDir = rd
              }
          ]
      Map.keys result `shouldBe` ["first-test", "second-test"]
      let firstSc = result Map.! "first-test"
      suiteConfigResourceDir firstSc `shouldBe` Just rd

  it "throws NoSuitesDeclared when every spec has an empty <root>/test" $
    withSystemTempDir "walk-empty" $ \dir -> do
      let root = dir </> [reldir|pkg|]
          rd = dir </> [reldir|resource|]
      createDirIfMissing True (root </> [reldir|test|])
      createDirIfMissing True rd
      result <-
        ( Exception.try ::
            IO (Map.Map a SuiteConfig) ->
            IO (Either SuitePkgWalkError (Map.Map a SuiteConfig))
        )
          $ walkSuitePkgs
            [ SuitePkgSpec
                { suitePkgSpecPname = "mypkg",
                  suitePkgSpecBuiltTestPkgRoot = root,
                  suitePkgSpecResourceDir = rd
                }
            ]
      case result of
        Left NoSuitesDeclared -> pure ()
        Left other -> expectationFailure ("wrong error: " ++ show other)
        Right m -> expectationFailure ("expected NoSuitesDeclared, got " ++ show (Map.keys m))

  it "throws NoSuitesDeclared when no specs are given" $ do
    result <-
      ( Exception.try ::
          IO (Map.Map a SuiteConfig) ->
          IO (Either SuitePkgWalkError (Map.Map a SuiteConfig))
      )
        $ walkSuitePkgs []
    case result of
      Left NoSuitesDeclared -> pure ()
      Left other -> expectationFailure ("wrong error: " ++ show other)
      Right m -> expectationFailure ("expected NoSuitesDeclared, got " ++ show (Map.keys m))

  it "silently ignores specs whose <root>/test directory does not exist" $
    withSystemTempDir "walk-missing-testdir" $ \dir -> do
      let rootA = dir </> [reldir|pkg-a|]
          rootB = dir </> [reldir|pkg-b|]
          rd = dir </> [reldir|resource|]
      createDirIfMissing True rootA -- no 'test' subdir
      createDirIfMissing True (rootB </> [reldir|test|])
      createDirIfMissing True rd
      B.writeFile (fromAbsFile (rootB </> [relfile|test/only|])) ""
      result <-
        walkSuitePkgs
          [ SuitePkgSpec "a" rootA rd,
            SuitePkgSpec "b" rootB rd
          ]
      Map.keys result `shouldBe` ["only"]

  it "throws DuplicateSuiteName when two specs produce the same key" $
    withSystemTempDir "walk-dup" $ \dir -> do
      let rootA = dir </> [reldir|pkg-a|]
          rootB = dir </> [reldir|pkg-b|]
          rd = dir </> [reldir|resource|]
      createDirIfMissing True (rootA </> [reldir|test|])
      createDirIfMissing True (rootB </> [reldir|test|])
      createDirIfMissing True rd
      B.writeFile (fromAbsFile (rootA </> [relfile|test/clash|])) ""
      B.writeFile (fromAbsFile (rootB </> [relfile|test/clash|])) ""
      result <-
        ( Exception.try ::
            IO (Map.Map a SuiteConfig) ->
            IO (Either SuitePkgWalkError (Map.Map a SuiteConfig))
        )
          $ walkSuitePkgs
            [ SuitePkgSpec "a" rootA rd,
              SuitePkgSpec "b" rootB rd
            ]
      case result of
        Left (DuplicateSuiteName name) -> name `shouldBe` "clash"
        Left other -> expectationFailure ("wrong error: " ++ show other)
        Right m -> expectationFailure ("expected DuplicateSuiteName, got " ++ show (Map.keys m))
