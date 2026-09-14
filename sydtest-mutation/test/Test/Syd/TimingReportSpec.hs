{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.TimingReportSpec (spec) where

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as T
import Path
import Path.IO (withSystemTempDir)
import Test.Syd
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId (..))
import Test.Syd.Mutation.Timing
import Test.Syd.Mutation.TimingReport
import Test.Syd.Mutation.TimingReport.Html (renderPhaseTimingSummaryHtml)
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Text.Colour (TerminalCapabilities (..), renderChunksText, unlinesChunks)

spec :: Spec
spec = do
  describe "ChildTiming" $ do
    genValidSpec @ChildTiming
    jsonSpec @ChildTiming

  describe "ChildRunOutcome" $ do
    genValidSpec @ChildRunOutcome
    jsonSpec @ChildRunOutcome

  describe "MutationChildTiming" $ do
    genValidSpec @MutationChildTiming
    jsonSpec @MutationChildTiming

  describe "MutationPhaseTiming" $ do
    genValidSpec @MutationPhaseTiming
    jsonSpec @MutationPhaseTiming

    it "round-trips through its file in a directory" $
      forAllValid $ \phaseTiming ->
        withSystemTempDir "mutation-phase-timing" $ \dir -> do
          writeMutationPhaseTiming dir phaseTiming
          readMutationPhaseTiming dir `shouldReturn` Right phaseTiming

    it "golden JSON" $
      pureGoldenLazyByteStringFile "test_resources/mutation-phase-timing.json" $
        encodePretty exampleMutationPhaseTiming

  describe "CoverageChildTiming" $ do
    genValidSpec @CoverageChildTiming
    jsonSpec @CoverageChildTiming

  describe "CoveragePhaseTiming" $ do
    genValidSpec @CoveragePhaseTiming
    jsonSpec @CoveragePhaseTiming

    it "round-trips through its file in a directory" $
      forAllValid $ \phaseTiming ->
        withSystemTempDir "coverage-phase-timing" $ \dir -> do
          writeCoveragePhaseTiming dir phaseTiming
          readCoveragePhaseTiming dir `shouldReturn` Right phaseTiming

  describe "renderStackedBar" $ do
    it "is blank for an empty breakdown" $
      renderStackedBar 10 100 mempty `shouldBe` "          "

    it "fills the width when the breakdown is the whole total" $
      renderStackedBar 10 100 mempty {costTestNanos = 100} `shouldBe` "██████████"

    it "draws each cost in its own shade, in order" $
      renderStackedBar
        8
        100
        CostBreakdown
          { costProcessNanos = 25,
            costSetupNanos = 25,
            costTestNanos = 25,
            costUnattributedNanos = 25
          }
        `shouldBe` "▓▓▒▒██░░"

    it "pads to the full width so columns after it line up" $
      forAllValid $ \cb ->
        T.length (renderStackedBar 12 (costTotalNanos cb) cb) `shouldBe` 12

    it "is blank when there is no total to be a fraction of" $
      forAllValid $ \cb ->
        renderStackedBar 6 0 cb `shouldBe` "      "

  describe "renderDurationNanos" $ do
    it "renders sub-microseconds in nanoseconds" $
      renderDurationNanos 999 `shouldBe` "999ns"

    it "renders sub-milliseconds in microseconds" $
      renderDurationNanos 1_500 `shouldBe` "1.5us"

    it "renders sub-seconds in milliseconds" $
      renderDurationNanos 1_500_000 `shouldBe` "1.5ms"

    it "renders sub-minutes in seconds" $
      renderDurationNanos 1_500_000_000 `shouldBe` "1.50s"

    it "renders sub-hours in minutes and seconds" $
      renderDurationNanos 90_000_000_000 `shouldBe` "1m30s"

    it "renders hours in hours, minutes and seconds" $
      renderDurationNanos 3_723_000_000_000 `shouldBe` "1h02m03s"

  describe "childCosts" $ do
    it "accounts for the whole wall time" $
      forAllValid $ \wallNanos ->
        forAllValid $ \mInner ->
          costTotalNanos (childCosts wallNanos mInner) `shouldBe` wallNanos

    it "attributes everything to the process when the child reported nothing" $
      forAllValid $ \wallNanos ->
        childCosts wallNanos Nothing
          `shouldBe` CostBreakdown
            { costProcessNanos = 0,
              costSetupNanos = 0,
              costTestNanos = 0,
              costUnattributedNanos = wallNanos
            }

    it "splits a child that spent all its forest time in tests into process and tests" $
      childCosts
        10_000_000_000
        ( Just
            ChildTiming
              { childTimingForestNanos = 6_000_000_000,
                childTimingTestNanos = 6_000_000_000,
                childTimingTestsRun = 3
              }
        )
        `shouldBe` CostBreakdown
          { costProcessNanos = 4_000_000_000,
            costSetupNanos = 0,
            costTestNanos = 6_000_000_000,
            costUnattributedNanos = 0
          }

  describe "summariseMutationPhase" $ do
    it "sums the children's wall time" $
      forAllValid $ \phaseTiming ->
        phaseTimingSummaryChildWallNanos (summariseMutationPhase phaseTiming)
          `shouldBe` sum
            (map mutationChildTimingWallNanos (mutationPhaseTimingChildren phaseTiming))

    it "keeps every child in the per-child listing" $
      forAllValid $ \phaseTiming ->
        length (phaseTimingSummaryEntries (summariseMutationPhase phaseTiming))
          `shouldBe` length (mutationPhaseTimingChildren phaseTiming)

    it "groups by mutation only when more than one suite ran" $ do
      let dimensionsOf =
            map timingDimensionName
              . phaseTimingSummaryDimensions
              . summariseMutationPhase
          oneSuite =
            exampleMutationPhaseTiming
              { mutationPhaseTimingChildren =
                  map
                    (\c -> c {mutationChildTimingSuite = "foo-test"})
                    (mutationPhaseTimingChildren exampleMutationPhaseTiming)
              }
      dimensionsOf exampleMutationPhaseTiming
        `shouldBe` ["suite", "outcome", "operator", "module", "mutation"]
      dimensionsOf oneSuite `shouldBe` ["suite", "outcome", "operator", "module"]

  describe "summariseCoveragePhase" $
    it "keeps every child in the per-child listing" $
      forAllValid $ \phaseTiming ->
        length (phaseTimingSummaryEntries (summariseCoveragePhase phaseTiming))
          `shouldBe` length (coveragePhaseTimingChildren phaseTiming)

  describe "renderPhaseTimingSummary" $ do
    it "golden mutation-phase report" $
      pureGoldenTextFile "test_resources/mutation-phase-timing.txt" $
        renderChunksText
          WithoutColours
          ( unlinesChunks
              (renderPhaseTimingSummary 25 (summariseMutationPhase exampleMutationPhaseTiming))
          )

    it "golden abridged mutation-phase report" $
      pureGoldenTextFile "test_resources/mutation-phase-timing-abridged.txt" $
        renderChunksText
          WithoutColours
          ( unlinesChunks
              (renderPhaseTimingSummary 1 (summariseMutationPhase exampleMutationPhaseTiming))
          )

    it "golden coverage-phase report" $
      pureGoldenTextFile "test_resources/coverage-phase-timing.txt" $
        renderChunksText
          WithoutColours
          ( unlinesChunks
              (renderPhaseTimingSummary 25 (summariseCoveragePhase exampleCoveragePhaseTiming))
          )

  describe "renderPhaseTimingSummaryHtml" $ do
    it "golden mutation-phase page" $
      pureGoldenTextFile "test_resources/mutation-phase-timing.html" $
        renderPhaseTimingSummaryHtml (summariseMutationPhase exampleMutationPhaseTiming)

    it "golden coverage-phase page" $
      pureGoldenTextFile "test_resources/coverage-phase-timing.html" $
        renderPhaseTimingSummaryHtml (summariseCoveragePhase exampleCoveragePhaseTiming)

    it "escapes markup in a label rather than emitting it" $
      let hostile =
            exampleMutationPhaseTiming
              { mutationPhaseTimingChildren =
                  [ c {mutationChildTimingModule = "Foo.<script>&\"'"}
                  | c <- mutationPhaseTimingChildren exampleMutationPhaseTiming
                  ]
              }
          page = renderPhaseTimingSummaryHtml (summariseMutationPhase hostile)
       in do
            T.isInfixOf "<script>&\"'" page `shouldBe` False
            T.isInfixOf "Foo.&lt;script&gt;&amp;&quot;&#39;" page `shouldBe` True

-- | A mutation phase with one child of each outcome, including one that
-- reported no inner timing, so the golden report exercises every branch of
-- the renderer.
exampleMutationPhaseTiming :: MutationPhaseTiming
exampleMutationPhaseTiming =
  MutationPhaseTiming
    { mutationPhaseTimingWallNanos = 45_000_000_000,
      mutationPhaseTimingJobs = 4,
      mutationPhaseTimingUncovered = 2,
      mutationPhaseTimingChildren =
        [ MutationChildTiming
            { mutationChildTimingId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
              mutationChildTimingSuite = "foo-test",
              mutationChildTimingOperator = "ArithOp",
              mutationChildTimingModule = "Foo.Bar",
              mutationChildTimingSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
              mutationChildTimingLine = 5,
              mutationChildTimingOutcome = ChildKilled,
              mutationChildTimingWallNanos = 12_000_000_000,
              mutationChildTimingCoveringTests = 7,
              mutationChildTimingInner =
                Just
                  ChildTiming
                    { childTimingForestNanos = 9_500_000_000,
                      childTimingTestNanos = 500_000_000,
                      childTimingTestsRun = 1
                    }
            },
          MutationChildTiming
            { mutationChildTimingId = MutationId ["Foo.Bar", "BoolOp", "12", "8", "10"],
              mutationChildTimingSuite = "foo-test",
              mutationChildTimingOperator = "BoolOp",
              mutationChildTimingModule = "Foo.Bar",
              mutationChildTimingSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
              mutationChildTimingLine = 12,
              mutationChildTimingOutcome = ChildSurvived,
              mutationChildTimingWallNanos = 20_000_000_000,
              mutationChildTimingCoveringTests = 7,
              mutationChildTimingInner =
                Just
                  ChildTiming
                    { childTimingForestNanos = 17_000_000_000,
                      childTimingTestNanos = 8_000_000_000,
                      childTimingTestsRun = 7
                    }
            },
          MutationChildTiming
            { mutationChildTimingId = MutationId ["Foo.Qux", "ConstBool", "40", "3", "7"],
              mutationChildTimingSuite = "qux-test",
              mutationChildTimingOperator = "ConstBool",
              mutationChildTimingModule = "Foo.Qux",
              mutationChildTimingSourceFile = Nothing,
              mutationChildTimingLine = 40,
              mutationChildTimingOutcome = ChildTimedOut,
              mutationChildTimingWallNanos = 30_000_000_000,
              mutationChildTimingCoveringTests = 2,
              mutationChildTimingInner = Nothing
            }
        ]
    }

exampleCoveragePhaseTiming :: CoveragePhaseTiming
exampleCoveragePhaseTiming =
  CoveragePhaseTiming
    { coveragePhaseTimingWallNanos = 20_000_000_000,
      coveragePhaseTimingJobs = 4,
      coveragePhaseTimingChildren =
        [ CoverageChildTiming
            { coverageChildTimingSuite = "foo-test",
              coverageChildTimingTestId = TestId (("add", 0) :| [("adds two numbers", 0)]),
              coverageChildTimingWallNanos = 11_000_000_000,
              coverageChildTimingAttempts = 1,
              coverageChildTimingMutationsCovered = 12,
              coverageChildTimingInner =
                Just
                  ChildTiming
                    { childTimingForestNanos = 9_000_000_000,
                      childTimingTestNanos = 250_000_000,
                      childTimingTestsRun = 1
                    }
            },
          CoverageChildTiming
            { coverageChildTimingSuite = "foo-test",
              coverageChildTimingTestId = TestId (("add", 0) :| [("commutativity", 1)]),
              coverageChildTimingWallNanos = 25_000_000_000,
              coverageChildTimingAttempts = 3,
              coverageChildTimingMutationsCovered = 12,
              coverageChildTimingInner =
                Just
                  ChildTiming
                    { childTimingForestNanos = 21_000_000_000,
                      childTimingTestNanos = 1_000_000_000,
                      childTimingTestsRun = 1
                    }
            }
        ]
    }
