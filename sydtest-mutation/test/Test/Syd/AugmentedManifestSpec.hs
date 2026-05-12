{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.AugmentedManifestSpec (spec) where

import Data.Aeson (encode)
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
        encode exampleMutationManifest

  describe "AugmentedManifest" $
    it "golden JSON" $
      pureGoldenLazyByteStringFile "test_resources/augmented-manifest.json" $
        encode exampleAugmentedManifest

  describe "MutationRunReport" $
    it "golden JSON" $
      pureGoldenLazyByteStringFile "test_resources/mutation-run-report.json" $
        encode exampleMutationRunReport

exampleMutationManifest :: MutationManifest
exampleMutationManifest =
  MutationManifest
    [ MutationRecord
        { mutRecId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
          mutRecOperator = "ArithOp",
          mutRecOriginal = "+",
          mutRecReplacement = "-",
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
          mutRecSourceFile = Nothing,
          mutRecSourceLine = Nothing,
          mutRecMutatedLine = Nothing,
          mutRecContextBefore = [],
          mutRecContextAfter = [],
          mutRecCoveringTests = Nothing
        }
    ]

exampleAugmentedManifest :: AugmentedManifest
exampleAugmentedManifest =
  AugmentedManifest
    [ AugmentedMutationRecord
        { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
          augmentedMutationRecordOperator = "ArithOp",
          augmentedMutationRecordOriginal = "+",
          augmentedMutationRecordReplacement = "-",
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
    ]

exampleMutationRunReport :: MutationRunReport
exampleMutationRunReport =
  MutationRunReport
    { mutationRunReportKilled = 5,
      mutationRunReportSurvived = 1,
      mutationRunReportUncovered = 2,
      mutationRunReportSurvivors =
        [ SurvivedMutation
            { survivedMutationRecord =
                AugmentedMutationRecord
                  { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
                    augmentedMutationRecordOperator = "ArithOp",
                    augmentedMutationRecordOriginal = "+",
                    augmentedMutationRecordReplacement = "-",
                    augmentedMutationRecordSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
                    augmentedMutationRecordSourceLine = Just "  result = x + y",
                    augmentedMutationRecordMutatedLine = Just "  result = x - y",
                    augmentedMutationRecordContextBefore = ["add :: Int -> Int -> Int", "add x y ="],
                    augmentedMutationRecordContextAfter = ["  in result"],
                    augmentedMutationRecordCoveringTests =
                      [ TestId [("add", 0), ("adds two numbers", 0)]
                      ]
                  },
              survivedMutationLogFile = $(mkRelFile "children/Foo.Bar-ArithOp-5-14-15.txt")
            }
        ]
    }
