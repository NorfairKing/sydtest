{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.AugmentedManifestSpec (spec) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Path
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
import Test.Syd.Mutation.Manifest (MutationManifest (..), MutationRecord (..))
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId (..))
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  describe "AugmentedMutationRecord" $ do
    genValidSpec @AugmentedMutationRecord
    jsonSpec @AugmentedMutationRecord

  describe "MutationManifest" $
    it "golden JSON" $
      pureGoldenLazyByteStringFile "test_resources/mutation-manifest.json" $
        encodePretty exampleMutationManifest

  describe "AugmentedManifest" $
    it "golden JSON" $
      pureGoldenLazyByteStringFile "test_resources/augmented-manifest.json" $
        encodePretty exampleAugmentedManifest

  describe "MutationRunReport" $
    it "golden JSON" $
      pureGoldenLazyByteStringFile "test_resources/mutation-run-report.json" $
        encodePretty exampleMutationRunReport

exampleMutationManifest :: MutationManifest
exampleMutationManifest =
  MutationManifest
    [ MutationRecord
        { mutRecId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
          mutRecOperator = "ArithOp",
          mutRecOriginal = "+",
          mutRecReplacement = "-",
          mutRecModule = "Foo.Bar",
          mutRecLine = 5,
          mutRecColStart = 14,
          mutRecColEnd = 15,
          mutRecSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
          mutRecSourceLine = Just "  result = x + y",
          mutRecMutatedLine = Just "  result = x - y",
          mutRecContextBefore = ["add :: Int -> Int -> Int", "add x y ="],
          mutRecContextAfter = ["  in result"],
          mutRecCoveringTests =
            Just
              [ TestId [("add", 0), ("adds two numbers", 0)],
                TestId [("add", 0), ("commutativity", 1)]
              ]
        },
      MutationRecord
        { mutRecId = MutationId ["Foo.Bar", "BoolOp", "12", "8", "10"],
          mutRecOperator = "BoolOp",
          mutRecOriginal = "&&",
          mutRecReplacement = "||",
          mutRecModule = "Foo.Bar",
          mutRecLine = 12,
          mutRecColStart = 8,
          mutRecColEnd = 10,
          mutRecSourceFile = Nothing,
          mutRecSourceLine = Nothing,
          mutRecMutatedLine = Nothing,
          mutRecContextBefore = [],
          mutRecContextAfter = [],
          mutRecCoveringTests = Nothing
        }
    ]

exampleAugmentedRecord :: AugmentedMutationRecord
exampleAugmentedRecord =
  AugmentedMutationRecord
    { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
      augmentedMutationRecordOperator = "ArithOp",
      augmentedMutationRecordOriginal = "+",
      augmentedMutationRecordReplacement = "-",
      augmentedMutationRecordModule = "Foo.Bar",
      augmentedMutationRecordLine = 5,
      augmentedMutationRecordColStart = 14,
      augmentedMutationRecordColEnd = 15,
      augmentedMutationRecordSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
      augmentedMutationRecordSourceLine = Just "  result = x + y",
      augmentedMutationRecordMutatedLine = Just "  result = x - y",
      augmentedMutationRecordContextBefore = ["add :: Int -> Int -> Int", "add x y ="],
      augmentedMutationRecordContextAfter = ["  in result"],
      augmentedMutationRecordCoveringTests =
        [ TestId [("add", 0), ("adds two numbers", 0)],
          TestId [("add", 0), ("commutativity", 1)]
        ]
    }

exampleAugmentedManifest :: AugmentedManifest
exampleAugmentedManifest =
  AugmentedManifest [exampleAugmentedRecord]

exampleMutationRunReport :: MutationRunReport
exampleMutationRunReport =
  MutationRunReport
    { mutationRunReportKilled = 5,
      mutationRunReportSurvived = 1,
      mutationRunReportUncovered = 1,
      mutationRunReportSurvivors =
        [ SurvivedMutation
            { survivedMutationRecord = exampleAugmentedRecord,
              survivedMutationLogFile = $(mkRelFile "children/Foo.Bar-ArithOp-5-14-15.txt")
            }
        ],
      mutationRunReportUncoveredMutations =
        [ UncoveredMutation
            { uncoveredMutationRecord =
                exampleAugmentedRecord
                  { augmentedMutationRecordId = MutationId ["Foo.Bar", "BoolOp", "12", "8", "10"],
                    augmentedMutationRecordOperator = "BoolOp",
                    augmentedMutationRecordOriginal = "&&",
                    augmentedMutationRecordReplacement = "||",
                    augmentedMutationRecordLine = 12,
                    augmentedMutationRecordColStart = 8,
                    augmentedMutationRecordColEnd = 10,
                    augmentedMutationRecordSourceFile = Nothing,
                    augmentedMutationRecordSourceLine = Nothing,
                    augmentedMutationRecordMutatedLine = Nothing,
                    augmentedMutationRecordContextBefore = [],
                    augmentedMutationRecordContextAfter = [],
                    augmentedMutationRecordCoveringTests = []
                  }
            }
        ]
    }
