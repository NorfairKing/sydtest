{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.Mutation.Driver.MutateSpec (spec) where

import qualified Control.Exception as Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Path (fromAbsFile, parseAbsDir, relfile, (</>))
import Path.IO (canonicalizePath, getPermissions, setOwnerExecutable, setPermissions, withSystemTempDir)
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
    writeAugmentedManifestFile,
  )
import Test.Syd.Mutation.Driver.Mutate (UnknownCoveringSuite (..), runMutationMode)
import Test.Syd.Mutation.Driver.OptParse (SuiteConfig (..))
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId (..))

spec :: Spec
spec = describe "runMutationMode" $ do
  it "runs each mutation child in its suite's resource directory" $
    -- Regression test: the diff runner used to spawn mutation children
    -- with no working directory, so they ran in the invocation dir instead
    -- of the suite's resource dir that the coverage phase recorded covering
    -- tests against — producing false survivors.  Here a stand-in suite exe
    -- records its working directory; it must equal the configured resource
    -- dir, not the test process's own working dir.
    withSystemTempDir "mutate-resdir-manifest" $ \manifestDir ->
      withSystemTempDir "mutate-resdir-out" $ \outDir ->
        withSystemTempDir "mutate-resdir-res" $ \resourceDir -> do
          let cwdFile = manifestDir </> [relfile|child-cwd.txt|]
              exeFile = manifestDir </> [relfile|suite-exe|]
          -- A suite exe that ignores the driver's args, records its working
          -- directory, and exits 0 (so the mutation "survives").
          writeFile
            (fromAbsFile exeFile)
            (unlines ["#!/bin/sh", "pwd -P > '" ++ fromAbsFile cwdFile ++ "'", "exit 0"])
          perms <- getPermissions exeFile
          setPermissions exeFile (setOwnerExecutable True perms)
          let record =
                AugmentedMutationRecord
                  { augmentedMutationRecordId = MutationId ["M", "Op", "1", "1", "2"],
                    augmentedMutationRecordOperator = "Op",
                    augmentedMutationRecordOriginal = "+",
                    augmentedMutationRecordReplacement = "-",
                    augmentedMutationRecordModule = "M",
                    augmentedMutationRecordLine = 1,
                    augmentedMutationRecordEndLine = 1,
                    augmentedMutationRecordColStart = 1,
                    augmentedMutationRecordColEnd = 2,
                    augmentedMutationRecordSourceFile = Nothing,
                    augmentedMutationRecordSourceLines = [],
                    augmentedMutationRecordMutatedLines = [],
                    augmentedMutationRecordContextBefore = [],
                    augmentedMutationRecordContextAfter = [],
                    augmentedMutationRecordCoveringTests =
                      Map.singleton "suite" [TestId (("t", 0) :| [])],
                    augmentedMutationRecordTimeoutMicros = 30000000,
                    augmentedMutationRecordBinding = Nothing,
                    augmentedMutationRecordMitigation = Nothing
                  }
          writeAugmentedManifestFile
            manifestDir
            (AugmentedManifest [AugmentedMutationGroup [record]])
          _ <-
            runMutationMode
              False
              False
              manifestDir
              outDir
              Nothing
              Nothing
              ( Map.singleton
                  "suite"
                  SuiteConfig
                    { suiteConfigExe = exeFile,
                      suiteConfigResourceDir = Just resourceDir
                    }
              )
          recorded <- readFile (fromAbsFile cwdFile)
          recordedDir <- parseAbsDir (takeWhile (/= '\n') recorded)
          canonRecorded <- canonicalizePath recordedDir
          canonResource <- canonicalizePath resourceDir
          canonRecorded `shouldBe` canonResource

  it "throws UnknownCoveringSuite when the manifest references a suite not in suiteExes" $
    withSystemTempDir "mutate-spec" $ \dir -> do
      let record =
            AugmentedMutationRecord
              { augmentedMutationRecordId = MutationId ["M", "Op", "1", "1", "2"],
                augmentedMutationRecordOperator = "Op",
                augmentedMutationRecordOriginal = "+",
                augmentedMutationRecordReplacement = "-",
                augmentedMutationRecordModule = "M",
                augmentedMutationRecordLine = 1,
                augmentedMutationRecordEndLine = 1,
                augmentedMutationRecordColStart = 1,
                augmentedMutationRecordColEnd = 2,
                augmentedMutationRecordSourceFile = Nothing,
                augmentedMutationRecordSourceLines = [],
                augmentedMutationRecordMutatedLines = [],
                augmentedMutationRecordContextBefore = [],
                augmentedMutationRecordContextAfter = [],
                augmentedMutationRecordCoveringTests = Map.singleton "absent-suite" [],
                augmentedMutationRecordTimeoutMicros = 30000000,
                augmentedMutationRecordBinding = Nothing,
                augmentedMutationRecordMitigation = Nothing
              }
      writeAugmentedManifestFile
        dir
        (AugmentedManifest [AugmentedMutationGroup [record]])
      result <-
        Exception.try $
          runMutationMode False False dir dir Nothing Nothing Map.empty
      case result of
        Left (UnknownCoveringSuite name _) -> name `shouldBe` "absent-suite"
        Right _ -> expectationFailure "expected UnknownCoveringSuite to be thrown"
