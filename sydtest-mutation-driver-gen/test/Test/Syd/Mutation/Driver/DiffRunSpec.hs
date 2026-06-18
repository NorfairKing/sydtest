{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Driver.DiffRunSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedMutationRecord (..),
    ControlTally (..),
    MutationGroupReport (..),
    MutationOutcome (..),
    MutationRunReport (..),
    MutationTally (..),
    SurvivedMutation (..),
  )
import Test.Syd.Mutation.Driver.AssertScore (assertScoreResult)
import Test.Syd.Mutation.Driver.DiffRun (renderDiffFinalSummary)
import Test.Syd.Mutation.Runtime (MutationId (..))
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
        { mutationRunReportMutations =
            MutationTally
              { mutationTallyKilled = 0,
                mutationTallySurvived = 0,
                mutationTallyTimedOut = 0,
                mutationTallyUncovered = 0,
                mutationTallySkipped = 0
              },
          mutationRunReportControls =
            ControlTally
              { controlTallyPassed = 0,
                controlTallyFailed = 0
              },
          mutationRunReportGroups = []
        }

    reportAllKilled =
      MutationRunReport
        { mutationRunReportMutations =
            MutationTally
              { mutationTallyKilled = 3,
                mutationTallySurvived = 0,
                mutationTallyTimedOut = 0,
                mutationTallyUncovered = 0,
                mutationTallySkipped = 0
              },
          mutationRunReportControls =
            ControlTally
              { controlTallyPassed = 0,
                controlTallyFailed = 0
              },
          mutationRunReportGroups = []
        }

    reportOneSurvivor =
      MutationRunReport
        { mutationRunReportMutations =
            MutationTally
              { mutationTallyKilled = 2,
                mutationTallySurvived = 1,
                mutationTallyTimedOut = 0,
                mutationTallyUncovered = 0,
                mutationTallySkipped = 0
              },
          mutationRunReportControls =
            ControlTally
              { controlTallyPassed = 0,
                controlTallyFailed = 0
              },
          mutationRunReportGroups =
            [ MutationGroupReport
                { mutationGroupReportOutcomes =
                    [OutcomeSurvived survivor]
                }
            ]
        }

    survivor =
      SurvivedMutation
        { survivedMutationRecord = survivorRecord,
          survivedMutationLogFile = Nothing
        }

    survivorRecord =
      AugmentedMutationRecord
        { augmentedMutationRecordId =
            MutationId ["Example.Lib", "BoolLit", "12", "8", "12"],
          augmentedMutationRecordOperator = "BoolLit",
          augmentedMutationRecordOriginal = "True",
          augmentedMutationRecordReplacement = "False",
          augmentedMutationRecordModule = "Example.Lib",
          augmentedMutationRecordLine = 12,
          augmentedMutationRecordEndLine = 12,
          augmentedMutationRecordColStart = 8,
          augmentedMutationRecordColEnd = 12,
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

    reportOneUncovered =
      MutationRunReport
        { mutationRunReportMutations =
            MutationTally
              { mutationTallyKilled = 2,
                mutationTallySurvived = 0,
                mutationTallyTimedOut = 0,
                mutationTallyUncovered = 1,
                mutationTallySkipped = 0
              },
          mutationRunReportControls =
            ControlTally
              { controlTallyPassed = 0,
                controlTallyFailed = 0
              },
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
          chunks = renderDiffFinalSummary n assertion report txtPath jsonPath
       in renderChunksText With24BitColours (unlinesChunks chunks)
