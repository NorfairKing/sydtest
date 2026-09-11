{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.Mutation.Driver.MutateSpec (spec) where

import qualified Control.Exception as Exception
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Path (fromAbsFile, fromRelFile, parseAbsDir, relfile, (</>))
import Path.IO (canonicalizePath, getPermissions, listDirRel, setOwnerExecutable, setPermissions, withSystemTempDir)
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
    ControlFailedMutation (..),
    ControlTally (..),
    MutationOutcome (..),
    MutationRunReport (..),
    MutationTally (..),
    mutationGroupReportOutcomes,
    writeAugmentedManifestFile,
  )
import Test.Syd.Mutation.Driver.Mutate (UnknownCoveringSuite (..), runMutationMode)
import Test.Syd.Mutation.Driver.OptParse (SuiteConfig (..))
import Test.Syd.Mutation.Manifest (controlOperatorName)
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

  it "reports a surviving control mutation as a passed control, excluded from the score" $
    -- A control (no-op) mutation is expected to survive.  Survival means the
    -- control passed, so it must NOT be counted as a surviving real mutation,
    -- and (with fail-fast on) it must NOT trip fail-fast and abort the run.
    withSystemTempDir "control-pass-manifest" $ \manifestDir ->
      withSystemTempDir "control-pass-out" $ \outDir -> do
        let exeFile = manifestDir </> [relfile|suite-exe|]
        writeFile (fromAbsFile exeFile) (unlines ["#!/bin/sh", "exit 0"])
        perms <- getPermissions exeFile
        setPermissions exeFile (setOwnerExecutable True perms)
        let record =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["M", "Control", "1", "1", "2", "no-op", "0"],
                  augmentedMutationRecordOperator = controlOperatorName,
                  augmentedMutationRecordOriginal = "no-op",
                  augmentedMutationRecordReplacement = "no-op",
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
        report <-
          runMutationMode
            True
            False
            manifestDir
            outDir
            Nothing
            Nothing
            ( Map.singleton
                "suite"
                SuiteConfig {suiteConfigExe = exeFile, suiteConfigResourceDir = Nothing}
            )
        controlTallyPassed (mutationRunReportControls report) `shouldBe` 1
        controlTallyFailed (mutationRunReportControls report) `shouldBe` 0
        mutationTallySurvived (mutationRunReportMutations report) `shouldBe` 0
        mutationTallyKilled (mutationRunReportMutations report) `shouldBe` 0

  it "records a killed control as a failed control, separate from the kill score" $
    -- A killed control means the suite is unsound (flaky/nondeterministic) or
    -- the harness is buggy.  It is recorded as a failed control (not a kill);
    -- a failed control fails the run, but runMutationMode itself still returns
    -- the report so the caller can decide the exit code.
    withSystemTempDir "control-fail-manifest" $ \manifestDir ->
      withSystemTempDir "control-fail-out" $ \outDir -> do
        let exeFile = manifestDir </> [relfile|suite-exe|]
        writeFile (fromAbsFile exeFile) (unlines ["#!/bin/sh", "exit 1"])
        perms <- getPermissions exeFile
        setPermissions exeFile (setOwnerExecutable True perms)
        let record =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["M", "Control", "1", "1", "2", "no-op", "0"],
                  augmentedMutationRecordOperator = controlOperatorName,
                  augmentedMutationRecordOriginal = "no-op",
                  augmentedMutationRecordReplacement = "no-op",
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
        report <-
          runMutationMode
            True
            False
            manifestDir
            outDir
            Nothing
            Nothing
            ( Map.singleton
                "suite"
                SuiteConfig {suiteConfigExe = exeFile, suiteConfigResourceDir = Nothing}
            )
        controlTallyFailed (mutationRunReportControls report) `shouldBe` 1
        controlTallyPassed (mutationRunReportControls report) `shouldBe` 0
        mutationTallyKilled (mutationRunReportMutations report) `shouldBe` 0
        mutationTallySurvived (mutationRunReportMutations report) `shouldBe` 0

  it "keeps the output of the run that killed a control" $
    -- A control is a no-op, so a kill means the suite is unsound.  The
    -- child's output is the only place the test that killed it is named,
    -- and without it the flaky test behind a control failure cannot be
    -- found from the report at all.
    withSystemTempDir "mutate-control-manifest" $ \manifestDir ->
      withSystemTempDir "mutate-control-out" $ \outDir -> do
        let exeFile = manifestDir </> [relfile|suite-exe|]
        writeFile
          (fromAbsFile exeFile)
          (unlines ["#!/bin/sh", "echo 'FAIL the flaky one'", "exit 1"])
        perms <- getPermissions exeFile
        setPermissions exeFile (setOwnerExecutable True perms)
        let record =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["M", "Control", "1", "1", "2"],
                  augmentedMutationRecordOperator = controlOperatorName,
                  augmentedMutationRecordOriginal = "no-op",
                  augmentedMutationRecordReplacement = "no-op",
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
        report <-
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
                    suiteConfigResourceDir = Nothing
                  }
            )
        let outcomes = concatMap mutationGroupReportOutcomes (mutationRunReportGroups report)
        case [cf | OutcomeControlFailed cf <- outcomes] of
          [cf] -> case controlFailedMutationLogFile cf of
            Nothing -> expectationFailure "the killed control kept no log to name its test"
            Just relFile -> do
              contents <- readFile (fromAbsFile (outDir </> relFile))
              contents `shouldContain` "FAIL the flaky one"
          other -> expectationFailure ("expected exactly one control failure, got " ++ show (length other))

  it "keeps no output for an ordinary killed mutation" $
    -- The overwhelming majority of a healthy run is kills, and their output
    -- says nothing, so keeping it would be a log per mutation.
    withSystemTempDir "mutate-killed-manifest" $ \manifestDir ->
      withSystemTempDir "mutate-killed-out" $ \outDir -> do
        let exeFile = manifestDir </> [relfile|suite-exe|]
        writeFile
          (fromAbsFile exeFile)
          (unlines ["#!/bin/sh", "echo 'killed it'", "exit 1"])
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
                    suiteConfigResourceDir = Nothing
                  }
            )
        logs <- listDirRel outDir
        filter (isInfixOf "control-failure-" . fromRelFile) (snd logs) `shouldBe` []
