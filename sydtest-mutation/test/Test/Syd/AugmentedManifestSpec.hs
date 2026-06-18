{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.AugmentedManifestSpec (spec) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Path
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
import Test.Syd.Mutation.Manifest (MutationGroup (..), MutationManifest (..), MutationRecord (..))
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId (..))
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  describe "AugmentedMutationRecord" $ do
    genValidSpec @AugmentedMutationRecord
    jsonSpec @AugmentedMutationRecord

  describe "SurvivedMutation" $ do
    genValidSpec @SurvivedMutation
    jsonSpec @SurvivedMutation

  describe "TimedOutMutation" $ do
    genValidSpec @TimedOutMutation
    jsonSpec @TimedOutMutation

  describe "UncoveredMutation" $ do
    genValidSpec @UncoveredMutation
    jsonSpec @UncoveredMutation

  describe "SkippedMutation" $ do
    genValidSpec @SkippedMutation
    jsonSpec @SkippedMutation

  describe "ControlFailedMutation" $ do
    genValidSpec @ControlFailedMutation
    jsonSpec @ControlFailedMutation

  describe "MutationOutcome" $ do
    genValidSpec @MutationOutcome
    jsonSpec @MutationOutcome

  describe "MutationGroupReport" $ do
    genValidSpec @MutationGroupReport
    jsonSpec @MutationGroupReport

  describe "MutationTally" $ do
    genValidSpec @MutationTally
    jsonSpec @MutationTally

  describe "ControlTally" $ do
    genValidSpec @ControlTally
    jsonSpec @ControlTally

  describe "MutationRunReport" $ do
    genValidSpec @MutationRunReport
    jsonSpec @MutationRunReport

  describe "MutationManifest" $
    it "golden JSON" $
      pureGoldenLazyByteStringFile "test_resources/mutation-manifest.json" $
        encodePretty $
          MutationManifest
            [ MutationGroup
                [ MutationRecord
                    { mutRecId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
                      mutRecOperator = "ArithOp",
                      mutRecOriginal = "+",
                      mutRecReplacement = "-",
                      mutRecModule = "Foo.Bar",
                      mutRecLine = 5,
                      mutRecEndLine = 5,
                      mutRecColStart = 14,
                      mutRecColEnd = 15,
                      mutRecSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
                      mutRecSourceLines = ["  result = x + y"],
                      mutRecMutatedLines = ["  result = x - y"],
                      mutRecContextBefore = ["add :: Int -> Int -> Int", "add x y ="],
                      mutRecContextAfter = ["  in result"],
                      mutRecCoveringTests =
                        Just $
                          Map.singleton
                            ""
                            [ TestId (("add", 0) :| [("adds two numbers", 0)]),
                              TestId (("add", 0) :| [("commutativity", 1)])
                            ],
                      mutRecBinding = Nothing,
                      mutRecMitigation = Nothing
                    }
                ],
              MutationGroup
                [ MutationRecord
                    { mutRecId = MutationId ["Foo.Bar", "BoolOp", "12", "8", "10"],
                      mutRecOperator = "BoolOp",
                      mutRecOriginal = "&&",
                      mutRecReplacement = "||",
                      mutRecModule = "Foo.Bar",
                      mutRecLine = 12,
                      mutRecEndLine = 12,
                      mutRecColStart = 8,
                      mutRecColEnd = 10,
                      mutRecSourceFile = Nothing,
                      mutRecSourceLines = [],
                      mutRecMutatedLines = [],
                      mutRecContextBefore = [],
                      mutRecContextAfter = [],
                      mutRecCoveringTests = Nothing,
                      mutRecBinding = Nothing,
                      mutRecMitigation = Nothing
                    }
                ]
            ]

  describe "AugmentedManifest" $ do
    genValidSpec @AugmentedManifest
    jsonSpec @AugmentedManifest

    it "golden JSON" $
      pureGoldenLazyByteStringFile "test_resources/augmented-manifest.json" $
        encodePretty $
          AugmentedManifest
            [ AugmentedMutationGroup
                [ AugmentedMutationRecord
                    { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
                      augmentedMutationRecordOperator = "ArithOp",
                      augmentedMutationRecordOriginal = "+",
                      augmentedMutationRecordReplacement = "-",
                      augmentedMutationRecordModule = "Foo.Bar",
                      augmentedMutationRecordLine = 5,
                      augmentedMutationRecordEndLine = 5,
                      augmentedMutationRecordColStart = 14,
                      augmentedMutationRecordColEnd = 15,
                      augmentedMutationRecordSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
                      augmentedMutationRecordSourceLines = ["  result = x + y"],
                      augmentedMutationRecordMutatedLines = ["  result = x - y"],
                      augmentedMutationRecordContextBefore = ["add :: Int -> Int -> Int", "add x y ="],
                      augmentedMutationRecordContextAfter = ["  in result"],
                      augmentedMutationRecordCoveringTests =
                        Map.singleton
                          ""
                          [ TestId (("add", 0) :| [("adds two numbers", 0)]),
                            TestId (("add", 0) :| [("commutativity", 1)])
                          ],
                      augmentedMutationRecordTimeoutMicros = 30000000,
                      augmentedMutationRecordBinding = Nothing,
                      augmentedMutationRecordMitigation = Nothing
                    }
                ]
            ]

  describe "mergeAugmentedManifests" $ do
    it "is idempotent" $
      forAllValid $ \m ->
        mergeAugmentedManifests m m `shouldBe` m

  describe "MutationRunReport" $
    it "golden JSON" $
      pureGoldenLazyByteStringFile "test_resources/mutation-run-report.json" $
        let survivor =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
                  augmentedMutationRecordOperator = "ArithOp",
                  augmentedMutationRecordOriginal = "+",
                  augmentedMutationRecordReplacement = "-",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 5,
                  augmentedMutationRecordEndLine = 5,
                  augmentedMutationRecordColStart = 14,
                  augmentedMutationRecordColEnd = 15,
                  augmentedMutationRecordSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
                  augmentedMutationRecordSourceLines = ["  result = x + y"],
                  augmentedMutationRecordMutatedLines = ["  result = x - y"],
                  augmentedMutationRecordContextBefore = ["add :: Int -> Int -> Int", "add x y ="],
                  augmentedMutationRecordContextAfter = ["  in result"],
                  augmentedMutationRecordCoveringTests =
                    Map.singleton
                      ""
                      [ TestId (("add", 0) :| [("adds two numbers", 0)]),
                        TestId (("add", 0) :| [("commutativity", 1)])
                      ],
                  augmentedMutationRecordTimeoutMicros = 30000000,
                  augmentedMutationRecordBinding = Nothing,
                  augmentedMutationRecordMitigation = Nothing
                }
            uncovered =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "BoolOp", "12", "8", "10"],
                  augmentedMutationRecordOperator = "BoolOp",
                  augmentedMutationRecordOriginal = "&&",
                  augmentedMutationRecordReplacement = "||",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 12,
                  augmentedMutationRecordEndLine = 12,
                  augmentedMutationRecordColStart = 8,
                  augmentedMutationRecordColEnd = 10,
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = [],
                  augmentedMutationRecordCoveringTests = Map.empty,
                  augmentedMutationRecordTimeoutMicros = 30000000,
                  augmentedMutationRecordBinding = Nothing,
                  augmentedMutationRecordMitigation = Nothing
                }
            control =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "Control", "5", "14", "15", "no-op", "0"],
                  augmentedMutationRecordOperator = "Control",
                  augmentedMutationRecordOriginal = "no-op",
                  augmentedMutationRecordReplacement = "no-op",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 5,
                  augmentedMutationRecordEndLine = 5,
                  augmentedMutationRecordColStart = 14,
                  augmentedMutationRecordColEnd = 15,
                  augmentedMutationRecordSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
                  augmentedMutationRecordSourceLines = ["  result = x + y"],
                  augmentedMutationRecordMutatedLines = ["  result = x + y"],
                  augmentedMutationRecordContextBefore = ["add :: Int -> Int -> Int", "add x y ="],
                  augmentedMutationRecordContextAfter = ["  in result"],
                  augmentedMutationRecordCoveringTests =
                    Map.singleton "" [TestId (("add", 0) :| [("adds two numbers", 0)])],
                  augmentedMutationRecordTimeoutMicros = 30000000,
                  augmentedMutationRecordBinding = Nothing,
                  augmentedMutationRecordMitigation = Nothing
                }
         in encodePretty
              MutationRunReport
                { mutationRunReportMutations =
                    MutationTally
                      { mutationTallyKilled = 5,
                        mutationTallySurvived = 1,
                        mutationTallyTimedOut = 0,
                        mutationTallyUncovered = 1,
                        mutationTallySkipped = 0
                      },
                  mutationRunReportControls =
                    ControlTally
                      { controlTallyPassed = 1,
                        controlTallyFailed = 1
                      },
                  mutationRunReportGroups =
                    [ MutationGroupReport
                        [ OutcomeSurvived
                            SurvivedMutation
                              { survivedMutationRecord = survivor,
                                survivedMutationLogFile = Just $(mkRelFile "children/Foo.Bar-ArithOp-5-14-15.txt")
                              }
                        ],
                      MutationGroupReport
                        [ OutcomeUncovered
                            UncoveredMutation
                              { uncoveredMutationRecord = uncovered
                              }
                        ],
                      MutationGroupReport [OutcomeControlPassed control],
                      MutationGroupReport
                        [ OutcomeControlFailed
                            ControlFailedMutation
                              { controlFailedMutationRecord = control,
                                controlFailedMutationLogFile = Just $(mkRelFile "children/Foo.Bar-Control-5-14-15.txt")
                              }
                        ]
                    ]
                }
