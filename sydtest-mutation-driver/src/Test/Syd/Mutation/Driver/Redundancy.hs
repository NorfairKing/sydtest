{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parent-side runner for the @redundancy@ subcommand: redundant-test
-- analysis.
--
-- The @coverage@ basis is pure post-processing of the cached augmented
-- manifest(s): it builds, per suite, the relation mapping each test to the
-- mutations it /reaches/, then runs 'analyzeRedundancy'.  No test runs happen,
-- so it is instant and needs no prior mutation run.  The @kill@ basis is in
-- "Test.Syd.Mutation.Driver.RedundancyKill".
module Test.Syd.Mutation.Driver.Redundancy
  ( runRedundancy,
    coverageRelations,
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
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
    mergeAugmentedManifests,
    readAugmentedManifestFile,
  )
import Test.Syd.Mutation.Driver.OptParse (RedundancySettings (..))
import Test.Syd.Mutation.Driver.RedundancyKill (runKillRedundancy)
import Test.Syd.Mutation.Redundancy
import Test.Syd.Mutation.Runtime (MutationId, renderMutationId)
import Test.Syd.Mutation.TestId (TestId, renderTestId)
import Text.Colour

-- | Run the @redundancy@ subcommand: compute the reports for the chosen basis,
-- then write @redundancy.json@\/@redundancy.txt@ and print the rendered block.
runRedundancy :: RedundancySettings -> IO ()
runRedundancy settings = do
  reports <- case redundancySettingBasis settings of
    BasisCoverage -> coverageReports settings
    BasisKill -> runKillRedundancy settings
  emitReports (redundancySettingOutDir settings) reports

-- | Write @redundancy.json@ + @redundancy.txt@ to the out dir and print the
-- rendered block to stdout.
emitReports :: Path Abs Dir -> [RedundancyReport] -> IO ()
emitReports outDir reports = do
  ensureDir outDir
  writeRedundancyReportFile outDir reports
  let renderedChunks = renderRedundancyReports reports
  writeRedundancyTxt renderedChunks outDir
  putChunksLocaleWith With8BitColours (unlinesChunks renderedChunks)

-- | Coverage-basis redundancy: read the cached augmented manifest(s), build the
-- per-suite reach relation, and analyse.  No test runs.
coverageReports :: RedundancySettings -> IO [RedundancyReport]
coverageReports RedundancySettings {redundancySettingCoverageDirs} = do
  manifests <-
    mapM
      (\dir -> readAugmentedManifestFile (dir </> [reldir|augmented|]))
      redundancySettingCoverageDirs
  let manifest = foldl' mergeAugmentedManifests (AugmentedManifest []) manifests
      relBySuite = coverageRelations manifest
  pure
    [ analyzeRedundancy BasisCoverage suite Map.empty rel
    | (suite, rel) <- Map.toList relBySuite
    ]

-- | Build the per-suite coverage relation from an augmented manifest: for each
-- suite, a map from each test to the set of mutations it reaches.
coverageRelations :: AugmentedManifest -> Map.Map Text (Map.Map TestId (Set MutationId))
coverageRelations (AugmentedManifest groups) =
  Map.fromListWith
    (Map.unionWith Set.union)
    [ (suite, Map.singleton tid (Set.singleton (augmentedMutationRecordId rec)))
    | AugmentedMutationGroup recs <- groups,
      rec <- recs,
      (suite, tids) <- Map.toList (augmentedMutationRecordCoveringTests rec),
      tid <- tids
    ]

-- | Write @redundancy.txt@ to the out dir.  Writes bytes directly so we do not
-- depend on the locale's encoding (the report contains em-dashes and
-- box-drawing characters), mirroring the mutation report writer.
writeRedundancyTxt :: [[Chunk]] -> Path Abs Dir -> IO ()
writeRedundancyTxt renderedChunks outDir =
  let renderedText = renderChunksText With8BitColours (unlinesChunks renderedChunks)
      reportFile = outDir </> [relfile|redundancy.txt|]
   in BS.writeFile (fromAbsFile reportFile) (TE.encodeUtf8 renderedText)

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
    basis = redundancyReportBasis report
    -- Verbs differ by basis: coverage measures reach, kill measures catch.
    catchVerb = case basis of
      BasisCoverage -> "reaches"
      BasisKill -> "catches"
    soleNoun = case basis of
      BasisCoverage -> "sole reacher"
      BasisKill -> "sole killer"
    basisLabel = case basis of
      BasisCoverage -> "coverage (approximate — reaches, may not catch)"
      BasisKill -> "kill (accurate)"

    headerLine =
      [ fore cyan (chunk "Redundant tests"),
        chunk " — basis: ",
        chunk basisLabel
      ]
    suiteLine =
      [[chunk "  suite: ", fore cyan (chunk (redundancyReportSuite report))] | not (T.null (redundancyReportSuite report))]

    bullet t = [chunk "    • ", chunk t]

    soleKillers = redundancyReportSoleKillers report
    soleKillersBlock =
      countHeader (Map.size soleKillers) ("Load-bearing (" <> soleNoun <> " — never remove)")
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
      countHeader (length subsumptions) ("Dominated tests (" <> catchVerb <> " a strict subset of another test)")
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
    -- A coloured "<Label>: <n>" header line.
    countHeader n label =
      [ [ chunk (label <> ": "),
          fore (if n == 0 then green else yellow) (chunk (T.pack (show n)))
        ]
      ]
