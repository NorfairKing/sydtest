{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Redundant-test reporting for the mutation phase.
--
-- The mutation phase already runs each mutation's covering tests; when it
-- collects each test's pass\/fail (a 'TestKillRow' per mutation per suite),
-- 'buildKillRelations' aggregates those rows into the per-suite
-- @test -> killed-mutations@ relation, 'analyzeRedundancy' turns each into a
-- 'RedundancyReport', and 'writeRedundancyArtifacts' writes\/prints them
-- alongside the mutation report.  No separate command or run is involved.
module Test.Syd.Mutation.Driver.Redundancy
  ( buildKillRelations,
    writeRedundancyArtifacts,
    renderRedundancyReports,
    renderRedundancyReport,
  )
where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Path
import Path.IO (ensureDir)
import Test.Syd.Mutation.AugmentedManifest (AugmentedMutationRecord (..))
import Test.Syd.Mutation.KillRow (TestKillRow (..))
import Test.Syd.Mutation.Redundancy
import Test.Syd.Mutation.Runtime (MutationId, renderMutationId)
import Test.Syd.Mutation.TestId (TestId, renderTestId)
import Text.Colour

-- | Aggregate the kill matrix into the per-suite @test -> killed-mutations@
-- relation.  Seeded with every covering test mapped to the empty set (so a test
-- that catches nothing still appears, as removable), then overlaid with the
-- mutations each test actually killed.  Restricted to the given runnable suites.
buildKillRelations ::
  Set Text ->
  [AugmentedMutationRecord] ->
  [(Text, MutationId, TestKillRow)] ->
  Map.Map Text (Map.Map TestId (Set MutationId))
buildKillRelations runnableSuites records rows =
  Map.unionWith (Map.unionWith Set.union) seedRel killsRel
  where
    seedRel =
      Map.fromListWith
        (Map.unionWith Set.union)
        [ (suite, Map.singleton tid Set.empty)
        | record <- records,
          (suite, tids) <- Map.toList (augmentedMutationRecordCoveringTests record),
          suite `Set.member` runnableSuites,
          tid <- tids
        ]
    killsRel =
      Map.fromListWith
        (Map.unionWith Set.union)
        [ (suite, Map.singleton tid (Set.singleton mid))
        | (suite, mid, TestKillRow killMap) <- rows,
          suite `Set.member` runnableSuites,
          (tid, killed) <- Map.toList killMap,
          killed
        ]

-- | Build the per-suite redundancy reports from the kill matrix, write
-- @redundancy.json@ + @redundancy.txt@ into the out dir, and print the rendered
-- block to stdout.
writeRedundancyArtifacts ::
  Path Abs Dir ->
  Set Text ->
  [AugmentedMutationRecord] ->
  [(Text, MutationId, TestKillRow)] ->
  IO ()
writeRedundancyArtifacts outDir runnableSuites records rows = do
  let relBySuite = buildKillRelations runnableSuites records rows
      reports =
        [ analyzeRedundancy suite Map.empty rel
        | (suite, rel) <- Map.toList relBySuite
        ]
      renderedChunks = renderRedundancyReports reports
  ensureDir outDir
  writeRedundancyReportFile outDir reports
  let renderedText = renderChunksText With8BitColours (unlinesChunks renderedChunks)
  BS.writeFile (fromAbsFile (outDir </> [relfile|redundancy.txt|])) (TE.encodeUtf8 renderedText)
  putChunksLocaleWith With8BitColours (unlinesChunks renderedChunks)

-- | Render every suite's redundancy report as a block of coloured lines.
renderRedundancyReports :: [RedundancyReport] -> [[Chunk]]
renderRedundancyReports [] =
  [[fore yellow (chunk "No test coverage found; nothing to analyse for redundancy.")]]
renderRedundancyReports reports =
  intercalateBlank (map renderRedundancyReport reports)
  where
    intercalateBlank = \case
      [] -> []
      [b] -> b
      (b : bs) -> b ++ [[]] ++ intercalateBlank bs

-- | Render one suite's redundancy report.
renderRedundancyReport :: RedundancyReport -> [[Chunk]]
renderRedundancyReport report =
  concat
    [ [headerLine],
      suiteLine,
      [[]],
      soleKillersBlock,
      equivBlock,
      subsumptionBlock,
      minimalSuiteBlock,
      redundantMutantsBlock
    ]
  where
    headerLine = [fore cyan (chunk "Redundant tests"), chunk " (by mutations caught)"]
    suiteLine =
      [ [chunk "  suite: ", fore cyan (chunk (redundancyReportSuite report))]
      | not (T.null (redundancyReportSuite report))
      ]

    bullet t = [chunk "    • ", chunk t]

    soleKillers = redundancyReportSoleKillers report
    soleKillersBlock =
      countHeader (Map.size soleKillers) "Load-bearing (sole killer — never remove)"
        ++ [ bullet (renderTestId t <> "  (" <> T.pack (show (length ms)) <> " mutation" <> plural (length ms) <> ")")
           | (t, ms) <- Map.toAscList soleKillers
           ]
        ++ blankAfter (not (Map.null soleKillers))

    equivClasses = redundancyReportEquivClasses report
    equivBlock =
      countHeader (length equivClasses) "Equivalent test groups (keep one of each)"
        ++ concat
          [ [chunk "    keep:      ", fore green (chunk (renderTestId keep))]
              : [[chunk "    redundant: ", chunk (renderTestId t)] | t <- rest]
          | grp <- equivClasses,
            (keep : rest) <- [grp]
          ]
        ++ blankAfter (not (null equivClasses))

    subsumptions = redundancyReportSubsumptions report
    subsumptionBlock =
      countHeader (length subsumptions) "Dominated tests (catch a strict subset of another test)"
        ++ [ [ chunk "    • ",
               chunk (renderTestId (subsumptionDominated s)),
               chunk "  ⊂  ",
               fore green (chunk (renderTestId (subsumptionDominator s)))
             ]
           | s <- subsumptions
           ]
        ++ blankAfter (not (null subsumptions))

    minimal = redundancyReportMinimalSuite report
    removable = redundancyReportRemovable report
    total = length minimal + length removable
    minimalSuiteBlock =
      [ [ chunk "Suggested minimal suite: ",
          fore green (chunk (T.pack (show (length minimal)))),
          chunk " of ",
          chunk (T.pack (show total)),
          chunk " tests  (removes ",
          fore yellow (chunk (T.pack (show (length removable)))),
          chunk "; the cover is not unique)"
        ]
      ]
        ++ [ [chunk "    removable: ", chunk (T.intercalate ", " (map renderTestId removable))]
           | not (null removable)
           ]
        ++ [[]]

    redundantMutants = redundancyReportRedundantMutants report
    redundantMutantsBlock =
      countHeader (length redundantMutants) "Redundant mutants (killed by identical test sets)"
        ++ [ [chunk "    • ", chunk (T.intercalate ", " (map renderMutationIdText grp))]
           | grp <- redundantMutants
           ]

    renderMutationIdText :: MutationId -> Text
    renderMutationIdText = T.pack . renderMutationId

    plural n = if n == 1 then "" else "s"
    blankAfter b = [[] | b]
    countHeader n label =
      [ [ chunk (label <> ": "),
          fore (if n == 0 then green else yellow) (chunk (T.pack (show n)))
        ]
      ]
