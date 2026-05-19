{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Implementation of the @assert-score@ subcommand: read @report.json@
-- from a report directory, print a one-line pass\/fail header followed
-- by the rendered report body, and exit 0 on success, 1 on a failed
-- assertion (any survived, or any uncovered when the uncovered assertion
-- is enabled).
module Test.Syd.Mutation.Driver.AssertScore
  ( runAssertScore,
    assertScoreResult,
    AssertScoreResult (..),
  )
where

import Data.Foldable (for_)
import qualified Data.Text as T
import Path
import Path.IO (doesFileExist, ensureDir)
import System.Directory (createFileLink)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stdout)
import Test.Syd.Mutation.AugmentedManifest
  ( MutationRunReport (..),
    readMutationRunReport,
  )
import Test.Syd.MutationMode.Common (renderMutationRunReport)
import Text.Colour
  ( Chunk,
    TerminalCapabilities (..),
    chunk,
    fore,
    green,
    hPutChunksLocaleWith,
    red,
    unlinesChunks,
  )

-- | The decision 'assert-score' renders from a 'MutationRunReport'.
data AssertScoreResult = AssertScoreResult
  { -- | True when the assertion is violated: at least one survivor, or
    -- (when uncovered is also asserted) at least one uncovered mutation.
    assertScoreFailed :: !Bool,
    -- | Pass/fail header line (e.g. @PASS: All 17 mutation(s) accounted for.@).
    assertScoreHeader :: ![Chunk]
  }
  deriving (Show, Eq)

-- | Pure decision logic: given the report and whether to fail on
-- uncovered mutations, produce the header chunks and a pass/fail flag.
-- The body of the printed output is 'renderMutationRunReport' applied
-- separately by 'runAssertScore'.
assertScoreResult :: Bool -> MutationRunReport -> AssertScoreResult
assertScoreResult assertNoneUncovered MutationRunReport {mutationRunReportKilled, mutationRunReportSurvived, mutationRunReportUncovered} =
  let failed =
        mutationRunReportSurvived > 0
          || (assertNoneUncovered && mutationRunReportUncovered > 0)
      total =
        mutationRunReportKilled
          + mutationRunReportSurvived
          + mutationRunReportUncovered
      header
        | failed =
            [ fore red (chunk "FAIL: "),
              chunk (T.pack (show mutationRunReportSurvived)),
              chunk " surviving, ",
              chunk (T.pack (show mutationRunReportUncovered)),
              chunk " uncovered out of ",
              chunk (T.pack (show total)),
              chunk " mutation(s)."
            ]
        | otherwise =
            [ fore green (chunk "PASS: "),
              chunk "All ",
              chunk (T.pack (show total)),
              chunk " mutation(s) accounted for."
            ]
   in AssertScoreResult {assertScoreFailed = failed, assertScoreHeader = header}

-- | Top-level entry point for the @assert-score@ subcommand: read
-- @<reportDir>/report.json@, print the header and rendered body, and
-- exit with code 1 on a failed assertion.  Exits with code 2 if
-- @report.json@ is missing — distinct from a normal assertion failure
-- so the Nix harness can tell the two apart.
--
-- When @mOutDir@ is 'Just', and the assertion passes, also symlink
-- @report.txt@ and @report.json@ from the report directory into the
-- output directory.  The Nix @assertMutationScore@ derivation uses
-- this to populate its @$out@ as a single subcommand invocation.
runAssertScore :: Bool -> Path Abs Dir -> Maybe (Path Abs Dir) -> IO ()
runAssertScore assertNoneUncovered reportDir mOutDir = do
  let jsonPath = reportDir </> [relfile|report.json|]
  exists <- doesFileExist jsonPath
  if not exists
    then do
      hPutChunksLocaleWith With8BitColours stdout $
        unlinesChunks
          [ [ fore red (chunk "assert-score: "),
              chunk (T.pack (fromAbsFile jsonPath)),
              chunk " does not exist"
            ]
          ]
      exitWith (ExitFailure 2)
    else do
      report <- readMutationRunReport reportDir
      let result = assertScoreResult assertNoneUncovered report
          body = renderMutationRunReport report
          txtPath = reportDir </> [relfile|report.txt|]
      hPutChunksLocaleWith With8BitColours stdout $
        unlinesChunks (assertScoreHeader result : [] : body)
      -- Print the path lines so they appear in the build log even when
      -- the body alone scrolls them off-screen.
      hPutChunksLocaleWith With8BitColours stdout $
        unlinesChunks
          [ [],
            [chunk "Full report:             ", chunk (T.pack (fromAbsFile txtPath))],
            [chunk "Machine-readable report: ", chunk (T.pack (fromAbsFile jsonPath))]
          ]
      if assertScoreFailed result
        then exitWith (ExitFailure 1)
        else for_ mOutDir (symlinkReportsInto reportDir)

-- | Symlink @report.txt@ and @report.json@ from the report directory
-- into the given output directory.  Creates @outDir@ if it does not
-- already exist.  Used by 'runAssertScore' when @--out-dir@ is set.
symlinkReportsInto :: Path Abs Dir -> Path Abs Dir -> IO ()
symlinkReportsInto reportDir outDir = do
  ensureDir outDir
  let txtSrc = reportDir </> [relfile|report.txt|]
      jsonSrc = reportDir </> [relfile|report.json|]
      txtDest = outDir </> [relfile|report.txt|]
      jsonDest = outDir </> [relfile|report.json|]
  createFileLink (fromAbsFile txtSrc) (fromAbsFile txtDest)
  createFileLink (fromAbsFile jsonSrc) (fromAbsFile jsonDest)
