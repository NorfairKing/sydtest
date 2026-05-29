{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Driver.DiffRunSpec (spec) where

import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
  ( MutationRunReport (..),
  )
import Test.Syd.Mutation.Driver.AssertScore (assertScoreResult)
import Test.Syd.Mutation.Driver.DiffRun (renderDiffFinalSummary)
import Text.Colour (TerminalCapabilities (..), renderChunksText, unlinesChunks)

-- | These golden files capture the final block of the diff runner's
-- stdout — what the CI build log ends with after a diff-scoped mutation
-- run.  Open one of the @.golden@ files in the review and you see the
-- exact PASS\/FAIL + report-paths block the user will see.
spec :: Spec
spec = describe "renderDiffFinalSummary" $ do
  it "renders an empty-selection PASS block" $
    pureGoldenTextFile
      "test_resources/diff-run/empty-selection.golden"
      (renderColoured numSelected reportAllZero)

  it "renders an all-killed PASS block" $
    pureGoldenTextFile
      "test_resources/diff-run/all-killed.golden"
      (renderColoured 3 reportAllKilled)

  it "renders a FAIL block with one survivor" $
    pureGoldenTextFile
      "test_resources/diff-run/one-survivor-fail.golden"
      (renderColoured 3 reportOneSurvivor)

  it "renders a FAIL block with one uncovered mutation" $
    pureGoldenTextFile
      "test_resources/diff-run/one-uncovered-fail.golden"
      (renderColoured 3 reportOneUncovered)
  where
    numSelected = 0

    reportAllZero =
      MutationRunReport
        { mutationRunReportKilled = 0,
          mutationRunReportSurvived = 0,
          mutationRunReportTimedOut = 0,
          mutationRunReportUncovered = 0,
          mutationRunReportSkipped = 0,
          mutationRunReportGroups = []
        }

    reportAllKilled =
      MutationRunReport
        { mutationRunReportKilled = 3,
          mutationRunReportSurvived = 0,
          mutationRunReportTimedOut = 0,
          mutationRunReportUncovered = 0,
          mutationRunReportSkipped = 0,
          mutationRunReportGroups = []
        }

    reportOneSurvivor =
      MutationRunReport
        { mutationRunReportKilled = 2,
          mutationRunReportSurvived = 1,
          mutationRunReportTimedOut = 0,
          mutationRunReportUncovered = 0,
          mutationRunReportSkipped = 0,
          mutationRunReportGroups = []
        }

    reportOneUncovered =
      MutationRunReport
        { mutationRunReportKilled = 2,
          mutationRunReportSurvived = 0,
          mutationRunReportTimedOut = 0,
          mutationRunReportUncovered = 1,
          mutationRunReportSkipped = 0,
          mutationRunReportGroups = []
        }

    -- Fixed paths so the golden files don't drift with tmp dir names.
    txtPath = "/run/report.txt"
    jsonPath = "/run/report.json"

    -- Render with full 24-bit colour capability so the goldens capture
    -- the exact ANSI escape sequences a reviewer would see in their
    -- terminal — not the colour-stripped version.
    renderColoured n report =
      let assertion = assertScoreResult True report
          chunks = renderDiffFinalSummary n assertion txtPath jsonPath
       in renderChunksText With24BitColours (unlinesChunks chunks)
