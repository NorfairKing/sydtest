{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.Mutation.RedundancySpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Syd
import Test.Syd.Mutation.KillRow (TestKillRow)
import Test.Syd.Mutation.Redundancy
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId (..))
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  describe "RedundancyBasis" $ do
    genValidSpec @RedundancyBasis
    jsonSpec @RedundancyBasis
  describe "Subsumption" $ do
    genValidSpec @Subsumption
    jsonSpec @Subsumption
  describe "RedundancyReport" $ do
    genValidSpec @RedundancyReport
    jsonSpec @RedundancyReport
  describe "TestKillRow" $ do
    genValidSpec @TestKillRow
    jsonSpec @TestKillRow

  describe "analyzeRedundancy" $ do
    let tid :: Text -> TestId
        tid t = TestId ((t, 0) :| [])
        mut :: String -> MutationId
        mut s = MutationId [s]
        a = tid "a"
        b = tid "b"
        c = tid "c"
        m1 = mut "m1"
        m2 = mut "m2"

    it "reports an equivalence class for two tests with identical kill sets" $ do
      -- A and B both kill {m1,m2}; C kills only {m1}.  Baselines make A faster
      -- than B, so A is the suggested representative.
      let baselines = Map.fromList [(a, 10), (b, 20), (c, 5)]
          rel =
            Map.fromList
              [ (a, Set.fromList [m1, m2]),
                (b, Set.fromList [m1, m2]),
                (c, Set.fromList [m1])
              ]
          report = analyzeRedundancy BasisKill "" baselines rel
      redundancyReportEquivClasses report `shouldBe` [[a, b]]
      -- C is strictly dominated by A (the faster of the two dominators).
      redundancyReportSubsumptions report
        `shouldBe` [Subsumption {subsumptionDominated = c, subsumptionDominator = a, subsumptionStrict = True}]
      -- A alone covers the universe; B and C are removable, ordered fastest first.
      redundancyReportMinimalSuite report `shouldBe` [a]
      redundancyReportRemovable report `shouldBe` [c, b]
      redundancyReportSoleKillers report `shouldBe` Map.empty

    it "detects a sole killer and treats a zero-kill test as removable" $ do
      -- A kills {m1,m2}, B kills {m2}, C kills nothing.
      let rel =
            Map.fromList
              [ (a, Set.fromList [m1, m2]),
                (b, Set.fromList [m2]),
                (c, Set.empty)
              ]
          report = analyzeRedundancy BasisKill "" Map.empty rel
      redundancyReportEquivClasses report `shouldBe` []
      redundancyReportSubsumptions report
        `shouldBe` [Subsumption {subsumptionDominated = b, subsumptionDominator = a, subsumptionStrict = True}]
      redundancyReportMinimalSuite report `shouldBe` [a]
      redundancyReportRemovable report `shouldBe` [b, c]
      -- A is the only test that catches m1.
      redundancyReportSoleKillers report `shouldBe` Map.fromList [(a, [m1])]

    it "groups mutants killed by an identical test set (the dual)" $ do
      let rel =
            Map.fromList
              [ (a, Set.fromList [m1, m2]),
                (b, Set.fromList [m1, m2])
              ]
          report = analyzeRedundancy BasisKill "" Map.empty rel
      redundancyReportEquivClasses report `shouldBe` [[a, b]]
      redundancyReportRedundantMutants report `shouldBe` [[m1, m2]]

    it "produces an empty report when no mutation is caught" $ do
      let rel = Map.fromList [(a, Set.empty), (b, Set.empty)]
          report = analyzeRedundancy BasisCoverage "suite" Map.empty rel
      redundancyReportEquivClasses report `shouldBe` []
      redundancyReportSubsumptions report `shouldBe` []
      redundancyReportMinimalSuite report `shouldBe` []
      -- Crucially: not every test is declared removable when there is nothing
      -- to cover.
      redundancyReportRemovable report `shouldBe` []
      redundancyReportSoleKillers report `shouldBe` Map.empty
      redundancyReportRedundantMutants report `shouldBe` []

    it "keeps the basis and suite name it was given" $ do
      let report = analyzeRedundancy BasisCoverage "money-test" Map.empty (Map.singleton a (Set.singleton m1))
      redundancyReportBasis report `shouldBe` BasisCoverage
      redundancyReportSuite report `shouldBe` "money-test"
      -- A single test covering the only mutant is the whole minimal suite.
      redundancyReportMinimalSuite report `shouldBe` [a]
      redundancyReportRemovable report `shouldBe` []
