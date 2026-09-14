{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Emitting a phase's timing report: the whole thing as a page, an abridged
-- copy in the build log.
module Test.Syd.Mutation.Driver.Timing
  ( emitPhaseTimingReport,
    buildLogTimingLimit,
  )
where

import Control.Monad (when)
import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as TE
import Path
import Path.IO (ensureDir)
import Test.Syd.Mutation.TimingReport
  ( PhaseTimingSummary (..),
    renderPhaseTimingSummary,
  )
import Test.Syd.Mutation.TimingReport.Html (renderPhaseTimingSummaryHtml)
import Text.Colour (TerminalCapabilities (..), putChunksUtf8With, unlinesChunks)

-- | How many rows of each grouped view and of the per-child listing go to the
-- build log.
--
-- A run with thousands of mutations would otherwise bury the mutation report
-- itself — the survivors are what a reader is looking for, and they print
-- after this.  The unabridged listing is in @timing.html@ next to the report,
-- which the abridged copy points at.
buildLogTimingLimit :: Int
buildLogTimingLimit = 25

-- | Write @<outDir>/timing.html@ with every row, and print the abridged
-- version to stdout.
--
-- Writes the page as bytes rather than through the locale's encoding: it
-- carries module names and test descriptions, which are not necessarily
-- ASCII, and a build sandbox does not reliably have a UTF-8 locale.
--
-- A phase that ran no children (the diff runner on a diff that selects
-- nothing, most often) has nothing to say, so it prints nothing rather than
-- an empty skeleton between the report and the caller's summary.  The page is
-- still written, so the output directory's shape does not depend on it.
emitPhaseTimingReport :: Path Abs Dir -> PhaseTimingSummary -> IO ()
emitPhaseTimingReport outDir summary = do
  ensureDir outDir
  SB.writeFile
    (fromAbsFile (outDir </> [relfile|timing.html|]))
    (TE.encodeUtf8 (renderPhaseTimingSummaryHtml summary))
  when (phaseTimingSummaryChildren summary > 0) $
    putChunksUtf8With
      With8BitColours
      (unlinesChunks ([] : renderPhaseTimingSummary buildLogTimingLimit summary))
