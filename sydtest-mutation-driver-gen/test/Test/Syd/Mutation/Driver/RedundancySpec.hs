{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Driver.RedundancySpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
  )
import Test.Syd.Mutation.Driver.Redundancy (coverageRelations, renderRedundancyReport, renderRedundancyReports)
import Test.Syd.Mutation.Driver.RedundancyKill (buildKillRelations)
import Test.Syd.Mutation.KillRow (TestKillRow (..))
import Test.Syd.Mutation.Redundancy
  ( RedundancyBasis (..),
    RedundancyReport (..),
    Subsumption (..),
  )
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId, parseTestIdFilterArg)
import Text.Colour (TerminalCapabilities (..), renderChunksText, unlinesChunks)

spec :: Spec
spec = do
  describe "coverageRelations" $
    it "builds the per-suite test -> reached-mutations relation" $ do
      let recM1 = mkRecord ["m1"] (Map.singleton "suiteA" [tid "Foo.a", tid "Foo.b"])
          recM2 = mkRecord ["m2"] (Map.singleton "suiteA" [tid "Foo.a"])
          manifest = AugmentedManifest [AugmentedMutationGroup [recM1, recM2]]
      coverageRelations manifest
        `shouldBe` Map.singleton
          "suiteA"
          ( Map.fromList
              [ (tid "Foo.a", Set.fromList [MutationId ["m1"], MutationId ["m2"]]),
                (tid "Foo.b", Set.fromList [MutationId ["m1"]])
              ]
          )

  describe "buildKillRelations" $
    it "seeds covering tests, overlays kills, and drops non-runnable suites" $ do
      let recM1 = mkRecord ["m1"] (Map.singleton "s" [tid "Foo.a", tid "Foo.b"])
          recM2 = mkRecord ["m2"] (Map.singleton "s" [tid "Foo.a"])
          rows =
            [ ("s", MutationId ["m1"], TestKillRow (Map.fromList [(tid "Foo.a", True), (tid "Foo.b", False)])),
              ("s", MutationId ["m2"], TestKillRow (Map.fromList [(tid "Foo.a", False)])),
              -- A row for a suite we did not run is ignored.
              ("other", MutationId ["m1"], TestKillRow (Map.singleton (tid "Foo.a") True))
            ]
      buildKillRelations (Set.singleton "s") [recM1, recM2] rows
        `shouldBe` Map.singleton
          "s"
          ( Map.fromList
              [ (tid "Foo.a", Set.singleton (MutationId ["m1"])),
                (tid "Foo.b", Set.empty)
              ]
          )

  describe "renderRedundancyReport" $ do
    it "renders a full kill-basis report" $
      pureGoldenTextFile
        "test_resources/redundancy/kill-full.golden"
        (renderColoured (renderRedundancyReport killReport))

    it "renders a coverage-basis report (approximate wording)" $
      pureGoldenTextFile
        "test_resources/redundancy/coverage-basic.golden"
        (renderColoured (renderRedundancyReport coverageReport))

    it "renders the empty-coverage message" $
      pureGoldenTextFile
        "test_resources/redundancy/empty.golden"
        (renderColoured (renderRedundancyReports []))
  where
    renderColoured chunks = renderChunksText With24BitColours (unlinesChunks chunks)

    killReport =
      RedundancyReport
        { redundancyReportBasis = BasisKill,
          redundancyReportSuite = "money-test",
          redundancyReportEquivClasses = [[tid "Money.deposit.a", tid "Money.deposit.b"]],
          redundancyReportSubsumptions =
            [ Subsumption
                { subsumptionDominated = tid "Money.add.commutes",
                  subsumptionDominator = tid "Money.add.laws",
                  subsumptionStrict = True
                }
            ],
          redundancyReportMinimalSuite = [tid "Money.add.laws", tid "Money.deposit.a"],
          redundancyReportRemovable = [tid "Money.add.commutes", tid "Money.deposit.b"],
          redundancyReportSoleKillers =
            Map.singleton (tid "Money.parse.roundtrips") [MutationId ["Currency", "ConstBool", "44"]],
          redundancyReportRedundantMutants =
            [[MutationId ["Amount", "IntLit", "131"], MutationId ["Amount", "IntLit", "197"]]]
        }

    coverageReport =
      RedundancyReport
        { redundancyReportBasis = BasisCoverage,
          redundancyReportSuite = "",
          redundancyReportEquivClasses = [],
          redundancyReportSubsumptions =
            [ Subsumption
                { subsumptionDominated = tid "Foo.b",
                  subsumptionDominator = tid "Foo.a",
                  subsumptionStrict = True
                }
            ],
          redundancyReportMinimalSuite = [tid "Foo.a"],
          redundancyReportRemovable = [tid "Foo.b"],
          redundancyReportSoleKillers = Map.empty,
          redundancyReportRedundantMutants = []
        }

-- | A minimal augmented record carrying only an id and its covering tests;
-- the other fields are irrelevant to 'coverageRelations'.  Duplicated here
-- rather than shared, per the project's test-helper guidance.
mkRecord :: [String] -> Map.Map Text [TestId] -> AugmentedMutationRecord
mkRecord idParts covering =
  AugmentedMutationRecord
    { augmentedMutationRecordId = MutationId idParts,
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
      augmentedMutationRecordCoveringTests = covering,
      augmentedMutationRecordTimeoutMicros = 30000000
    }

tid :: Text -> TestId
tid t = case parseTestIdFilterArg t of
  Just x -> x
  Nothing -> error ("invalid TestId in test: " ++ show t)
