{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Driver.MutateSpec (spec) where

import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import Path.IO (withSystemTempDir)
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
    writeAugmentedManifestFile,
  )
import Test.Syd.Mutation.Driver.Mutate (UnknownCoveringSuite (..), runMutationMode)
import Test.Syd.Mutation.Runtime (MutationId (..))

spec :: Spec
spec = describe "runMutationMode" $
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
                augmentedMutationRecordTimeoutMicros = 30000000
              }
      writeAugmentedManifestFile
        dir
        (AugmentedManifest [AugmentedMutationGroup [record]])
      result <-
        Exception.try $
          runMutationMode False (Just dir) Nothing Nothing Map.empty
      case result of
        Left (UnknownCoveringSuite name _) -> name `shouldBe` "absent-suite"
        Right _ -> expectationFailure "expected UnknownCoveringSuite to be thrown"
