{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Where a mutation run's time goes.
--
-- A mutation check spawns one child process per mutation per covering suite,
-- and a coverage run spawns one per leaf test.  Each of those children pays
-- three separate costs, and which one dominates decides what would actually
-- make the run faster:
--
-- * process startup — spawning the executable, RTS init, reading the
--   augmented manifest and building the spec forest.  Dominated by this, a
--   run gets faster by testing several mutations per child, not by making
--   the tests faster.
-- * suite setup — @around@\/@aroundAll@ resources (a tmp-postgres, a
--   webdriver session), paid once per child instead of once per suite.
--   Dominated by this, a run gets faster by sharing those resources.
-- * test execution — the leaf tests themselves.  Dominated by this, the
--   covering-test sets are too large or the tests are genuinely slow.
--
-- The parent measures each child's wall time; the child reports its own
-- 'ChildTiming' so the three can be told apart.  This module holds the
-- recorded data ('MutationPhaseTiming', 'CoveragePhaseTiming'), the pure
-- aggregation over it ('summariseMutationPhase', 'summariseCoveragePhase'),
-- and the rendering ('renderPhaseTimingSummary').
module Test.Syd.Mutation.TimingReport
  ( -- * Recorded timings
    ChildRunOutcome (..),
    MutationChildTiming (..),
    MutationPhaseTiming (..),
    CoverageChildTiming (..),
    CoveragePhaseTiming (..),

    -- * Reading and writing
    writeMutationPhaseTiming,
    readMutationPhaseTiming,
    writeCoveragePhaseTiming,
    readCoveragePhaseTiming,

    -- * Aggregation
    CostBreakdown (..),
    costTotalNanos,
    childCosts,
    TimingBucket (..),
    TimingDimension (..),
    TimingEntry (..),
    entryColumnNames,
    entryCell,
    PhaseTimingSummary (..),
    summariseMutationPhase,
    summariseCoveragePhase,

    -- * Rendering
    renderPhaseTimingSummary,
    renderDurationNanos,
    renderStackedBar,
    percentText,
  )
where

import Autodocodec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Containers.ListUtils (nubOrd)
import Data.GenValidity
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Data.List (sortOn)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import Path
import Path.IO (ensureDir)
import Test.Syd.Mutation.Manifest (relFileCodec)
import Test.Syd.Mutation.Runtime (MutationId, renderMutationId)
import Test.Syd.Mutation.TestId (TestId, renderTestId)
import Test.Syd.Mutation.Timing (ChildTiming (..))
import Text.Colour
import Text.Printf (printf)

-- | How a single child process ended.  Distinct from the report's
-- 'Test.Syd.Mutation.AugmentedManifest.MutationOutcome' because that is the
-- verdict for a mutation across every covering suite, while this is what one
-- child did — and a control's reinterpretation has not happened yet.
data ChildRunOutcome
  = ChildKilled
  | ChildSurvived
  | ChildTimedOut
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec ChildRunOutcome)

instance Validity ChildRunOutcome

instance GenValid ChildRunOutcome where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec ChildRunOutcome where
  codec =
    stringConstCodec $
      NE.fromList
        [ (ChildKilled, "killed"),
          (ChildSurvived, "survived"),
          (ChildTimedOut, "timed_out")
        ]

renderChildRunOutcome :: ChildRunOutcome -> Text
renderChildRunOutcome = \case
  ChildKilled -> "killed"
  ChildSurvived -> "survived"
  ChildTimedOut -> "timed out"

-- | One mutation child run, as the parent observed it.
data MutationChildTiming = MutationChildTiming
  { mutationChildTimingId :: !MutationId,
    mutationChildTimingSuite :: !Text,
    mutationChildTimingOperator :: !Text,
    mutationChildTimingModule :: !Text,
    mutationChildTimingSourceFile :: !(Maybe (Path Rel File)),
    mutationChildTimingLine :: !Word,
    mutationChildTimingOutcome :: !ChildRunOutcome,
    -- | Nanoseconds from spawning the child to reaping it.
    mutationChildTimingWallNanos :: !Word64,
    -- | How many tests the coverage phase recorded as covering this mutation
    -- in this suite.  Compared against the child's 'childTimingTestsRun',
    -- this says how much fail-fast saved.
    mutationChildTimingCoveringTests :: !Word,
    -- | What the child measured of itself, when it lived long enough to say.
    mutationChildTimingInner :: !(Maybe ChildTiming)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec MutationChildTiming)

instance Validity MutationChildTiming

instance GenValid MutationChildTiming where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec MutationChildTiming where
  codec =
    object "MutationChildTiming" $
      MutationChildTiming
        <$> requiredField' "id" .= mutationChildTimingId
        <*> requiredField' "suite" .= mutationChildTimingSuite
        <*> requiredField' "operator" .= mutationChildTimingOperator
        <*> requiredField' "module" .= mutationChildTimingModule
        <*> optionalFieldWith' "source_file" relFileCodec .= mutationChildTimingSourceFile
        <*> requiredField' "line" .= mutationChildTimingLine
        <*> requiredField' "outcome" .= mutationChildTimingOutcome
        <*> requiredField' "wall_nanos" .= mutationChildTimingWallNanos
        <*> requiredField' "covering_tests" .= mutationChildTimingCoveringTests
        <*> optionalField' "inner" .= mutationChildTimingInner

-- | Everything the mutation phase of one run recorded.
data MutationPhaseTiming = MutationPhaseTiming
  { -- | Nanoseconds the whole mutation phase took, wall-clock.
    mutationPhaseTimingWallNanos :: !Word64,
    -- | The child concurrency the phase actually ran at.
    mutationPhaseTimingJobs :: !Word,
    -- | Mutations that were never run because no test covers them.  They
    -- spawn no child, so they appear in no other field.
    mutationPhaseTimingUncovered :: !Word,
    mutationPhaseTimingChildren :: ![MutationChildTiming]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec MutationPhaseTiming)

instance Validity MutationPhaseTiming

instance GenValid MutationPhaseTiming where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec MutationPhaseTiming where
  codec =
    object "MutationPhaseTiming" $
      MutationPhaseTiming
        <$> requiredField' "wall_nanos" .= mutationPhaseTimingWallNanos
        <*> requiredField' "jobs" .= mutationPhaseTimingJobs
        <*> optionalFieldWithDefault' "uncovered" 0 .= mutationPhaseTimingUncovered
        <*> requiredField' "children" .= mutationPhaseTimingChildren

-- | One coverage child run, as the parent observed it.
data CoverageChildTiming = CoverageChildTiming
  { coverageChildTimingSuite :: !Text,
    coverageChildTimingTestId :: !TestId,
    -- | Nanoseconds across every attempt for this test, retries included.
    coverageChildTimingWallNanos :: !Word64,
    -- | Attempts this test needed; 1 means it succeeded first time.
    coverageChildTimingAttempts :: !Word,
    coverageChildTimingMutationsCovered :: !Word,
    -- | What the last (successful) attempt measured of itself.
    coverageChildTimingInner :: !(Maybe ChildTiming)
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec CoverageChildTiming)

instance Validity CoverageChildTiming

instance GenValid CoverageChildTiming where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec CoverageChildTiming where
  codec =
    object "CoverageChildTiming" $
      CoverageChildTiming
        <$> requiredField' "suite" .= coverageChildTimingSuite
        <*> requiredField' "test_id" .= coverageChildTimingTestId
        <*> requiredField' "wall_nanos" .= coverageChildTimingWallNanos
        <*> requiredField' "attempts" .= coverageChildTimingAttempts
        <*> requiredField' "mutations_covered" .= coverageChildTimingMutationsCovered
        <*> optionalField' "inner" .= coverageChildTimingInner

-- | Everything the coverage phase of one run recorded.
data CoveragePhaseTiming = CoveragePhaseTiming
  { coveragePhaseTimingWallNanos :: !Word64,
    coveragePhaseTimingJobs :: !Word,
    coveragePhaseTimingChildren :: ![CoverageChildTiming]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec CoveragePhaseTiming)

instance Validity CoveragePhaseTiming

instance GenValid CoveragePhaseTiming where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec CoveragePhaseTiming where
  codec =
    object "CoveragePhaseTiming" $
      CoveragePhaseTiming
        <$> requiredField' "wall_nanos" .= coveragePhaseTimingWallNanos
        <*> requiredField' "jobs" .= coveragePhaseTimingJobs
        <*> requiredField' "children" .= coveragePhaseTimingChildren

timingRelFile :: Path Rel File
timingRelFile = [relfile|timing.json|]

writeMutationPhaseTiming :: Path Abs Dir -> MutationPhaseTiming -> IO ()
writeMutationPhaseTiming dir t = do
  ensureDir dir
  LB.writeFile (fromAbsFile (dir </> timingRelFile)) (encodeJSONViaCodec t)

readMutationPhaseTiming :: Path Abs Dir -> IO (Either String MutationPhaseTiming)
readMutationPhaseTiming dir =
  eitherDecodeJSONViaCodec . LB.fromStrict <$> SB.readFile (fromAbsFile (dir </> timingRelFile))

writeCoveragePhaseTiming :: Path Abs Dir -> CoveragePhaseTiming -> IO ()
writeCoveragePhaseTiming dir t = do
  ensureDir dir
  LB.writeFile (fromAbsFile (dir </> timingRelFile)) (encodeJSONViaCodec t)

readCoveragePhaseTiming :: Path Abs Dir -> IO (Either String CoveragePhaseTiming)
readCoveragePhaseTiming dir =
  eitherDecodeJSONViaCodec . LB.fromStrict <$> SB.readFile (fromAbsFile (dir </> timingRelFile))

-- | A span of child wall time split into the three costs a child pays, plus
-- whatever could not be attributed.  The four always sum to the wall time
-- they were computed from, so shares are honest even when some children
-- never reported an inner timing.
data CostBreakdown = CostBreakdown
  { -- | Spawning the process, RTS init, reading the manifest, building the
    -- spec forest, printing output.
    costProcessNanos :: !Word64,
    -- | @around@\/@aroundAll@ setup and teardown around the tests.
    costSetupNanos :: !Word64,
    -- | The leaf tests themselves.
    costTestNanos :: !Word64,
    -- | Wall time of children that reported no inner timing (killed on their
    -- timeout, or crashed before writing it).
    costUnattributedNanos :: !Word64
  }
  deriving stock (Show, Eq, Generic)

instance Validity CostBreakdown

instance GenValid CostBreakdown where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance Semigroup CostBreakdown where
  a <> b =
    CostBreakdown
      { costProcessNanos = costProcessNanos a + costProcessNanos b,
        costSetupNanos = costSetupNanos a + costSetupNanos b,
        costTestNanos = costTestNanos a + costTestNanos b,
        costUnattributedNanos = costUnattributedNanos a + costUnattributedNanos b
      }

instance Monoid CostBreakdown where
  mempty =
    CostBreakdown
      { costProcessNanos = 0,
        costSetupNanos = 0,
        costTestNanos = 0,
        costUnattributedNanos = 0
      }

costTotalNanos :: CostBreakdown -> Word64
costTotalNanos CostBreakdown {..} =
  costProcessNanos + costSetupNanos + costTestNanos + costUnattributedNanos

-- | Split one child's wall time into the three costs.
--
-- The child's own numbers are clamped to the parent's wall time before being
-- subtracted, so the parts always sum to exactly the wall time even if a
-- child reported something impossible.
childCosts :: Word64 -> Maybe ChildTiming -> CostBreakdown
childCosts wallNanos = \case
  Nothing -> mempty {costUnattributedNanos = wallNanos}
  Just ChildTiming {childTimingForestNanos, childTimingTestNanos} ->
    let forest = min childTimingForestNanos wallNanos
        tests = min childTimingTestNanos forest
     in CostBreakdown
          { costProcessNanos = wallNanos - forest,
            costSetupNanos = forest - tests,
            costTestNanos = tests,
            costUnattributedNanos = 0
          }

-- | One row of a grouped view: how much of the phase's child time went to
-- everything sharing one label.
data TimingBucket = TimingBucket
  { timingBucketLabel :: !Text,
    timingBucketChildren :: !Word,
    timingBucketWallNanos :: !Word64,
    timingBucketCosts :: !CostBreakdown
  }
  deriving stock (Show, Eq, Generic)

-- | One grouped view of a phase's children, ordered slowest first.
data TimingDimension = TimingDimension
  { timingDimensionName :: !Text,
    timingDimensionBuckets :: ![TimingBucket]
  }
  deriving stock (Show, Eq, Generic)

-- | One child, for the per-child listing.
data TimingEntry = TimingEntry
  { -- | The child on one line, for the terminal listing.
    timingEntryLabel :: !Text,
    -- | The same identity broken into named cells, for a report that can
    -- give each its own sortable column.  Names are the column headers and
    -- are the same for every entry of a phase; 'entryColumnNames' derives
    -- the header row from them rather than requiring a separate declaration
    -- to be kept in step.
    timingEntryCells :: ![(Text, Text)],
    timingEntryWallNanos :: !Word64,
    timingEntryCosts :: !CostBreakdown
  }
  deriving stock (Show, Eq, Generic)

-- | The column headers for a phase's per-child listing, taken from its first
-- entry.  Empty when the phase ran no children.
entryColumnNames :: [TimingEntry] -> [Text]
entryColumnNames = \case
  [] -> []
  (e : _) -> map fst (timingEntryCells e)

-- | One entry's value under a column header, empty when it has no such cell.
entryCell :: Text -> TimingEntry -> Text
entryCell name e = fromMaybe "" (lookup name (timingEntryCells e))

-- | Everything the rendered timing report shows, computed from a phase's
-- recorded timings.
data PhaseTimingSummary = PhaseTimingSummary
  { -- | What this phase is called in the report header.
    phaseTimingSummaryLabel :: !Text,
    phaseTimingSummaryWallNanos :: !Word64,
    phaseTimingSummaryJobs :: !Word,
    phaseTimingSummaryChildren :: !Word,
    -- | Summed over every child; exceeds the phase wall time by the achieved
    -- parallelism.
    phaseTimingSummaryChildWallNanos :: !Word64,
    phaseTimingSummaryCosts :: !CostBreakdown,
    -- | Extra lines for the header block, as label\/value pairs.
    phaseTimingSummaryNotes :: ![(Text, Text)],
    phaseTimingSummaryDimensions :: ![TimingDimension],
    -- | Every child, slowest first.
    phaseTimingSummaryEntries :: ![TimingEntry]
  }
  deriving stock (Show, Eq, Generic)

-- | Group children by a label, summing their wall time and costs, slowest
-- bucket first.
bucketBy :: (a -> Text) -> (a -> Word64) -> (a -> CostBreakdown) -> [a] -> [TimingBucket]
bucketBy labelOf wallOf costsOf xs =
  sortOn (Down . timingBucketWallNanos) $
    map (\(l, (n, w, c)) -> TimingBucket l n w c) $
      Map.toList $
        Map.fromListWith
          (\(n1, w1, c1) (n2, w2, c2) -> (n1 + n2, w1 + w2, c1 <> c2))
          [(labelOf x, (1, wallOf x, costsOf x)) | x <- xs]

summariseMutationPhase :: MutationPhaseTiming -> PhaseTimingSummary
summariseMutationPhase MutationPhaseTiming {..} =
  let children = mutationPhaseTimingChildren
      costsOf c = childCosts (mutationChildTimingWallNanos c) (mutationChildTimingInner c)
      wallOf = mutationChildTimingWallNanos
      dimension n f = TimingDimension n (bucketBy f wallOf costsOf children)
      -- A mutation's location is what the reader needs to find the slow code;
      -- the module name is the fallback when the plugin recorded no file.
      locationOf c = case mutationChildTimingSourceFile c of
        Just p -> T.pack (fromRelFile p)
        Nothing -> mutationChildTimingModule c
      entryLabel c =
        T.concat
          [ mutationChildTimingOperator c,
            " at ",
            locationOf c,
            ":",
            T.pack (show (mutationChildTimingLine c)),
            " (",
            mutationChildTimingSuite c,
            ", ",
            renderChildRunOutcome (mutationChildTimingOutcome c),
            testsRunNote c,
            ")"
          ]
      -- Say how many of the covering tests actually ran, so a mutation whose
      -- child ran one cheap test out of two hundred is distinguishable from
      -- one that ran them all.
      testsRunNote c = case testsRunCell c of
        "" -> ""
        cell -> T.concat [", ", cell, " tests"]
      testsRunCell c = case mutationChildTimingInner c of
        Nothing -> ""
        Just ChildTiming {childTimingTestsRun} ->
          T.concat
            [ T.pack (show childTimingTestsRun),
              "/",
              T.pack (show (mutationChildTimingCoveringTests c))
            ]
   in PhaseTimingSummary
        { phaseTimingSummaryLabel = "mutation phase",
          phaseTimingSummaryWallNanos = mutationPhaseTimingWallNanos,
          phaseTimingSummaryJobs = mutationPhaseTimingJobs,
          phaseTimingSummaryChildren = fromIntegral (length children),
          phaseTimingSummaryChildWallNanos = sum (map wallOf children),
          phaseTimingSummaryCosts = foldMap costsOf children,
          phaseTimingSummaryNotes =
            [ ("Uncovered (no child run)", T.pack (show mutationPhaseTimingUncovered))
            | mutationPhaseTimingUncovered > 0
            ],
          phaseTimingSummaryDimensions =
            [ dimension "suite" mutationChildTimingSuite,
              dimension "outcome" (renderChildRunOutcome . mutationChildTimingOutcome),
              dimension "operator" mutationChildTimingOperator,
              dimension "module" mutationChildTimingModule
            ]
              -- One suite means one child per mutation, so a per-mutation
              -- grouping would repeat the per-child listing row for row.  It
              -- earns its place only when a mutation's cost is spread over
              -- several suites' children.
              ++ [ dimension "mutation" (T.pack . renderMutationId . mutationChildTimingId)
                 | length (nubOrd (map mutationChildTimingSuite children)) > 1
                 ],
          phaseTimingSummaryEntries =
            sortOn (Down . timingEntryWallNanos) $
              map
                ( \c ->
                    TimingEntry
                      { timingEntryLabel = entryLabel c,
                        -- No separate module column: the location and the
                        -- mutation id both already carry it, and every extra
                        -- column costs width in the per-child table.  It is
                        -- still a grouping dimension of its own.
                        timingEntryCells =
                          [ ("operator", mutationChildTimingOperator c),
                            ( "location",
                              T.concat
                                [ locationOf c,
                                  ":",
                                  T.pack (show (mutationChildTimingLine c))
                                ]
                            ),
                            ("suite", mutationChildTimingSuite c),
                            ("outcome", renderChildRunOutcome (mutationChildTimingOutcome c)),
                            ("tests run", testsRunCell c),
                            ("mutation", T.pack (renderMutationId (mutationChildTimingId c)))
                          ],
                        timingEntryWallNanos = wallOf c,
                        timingEntryCosts = costsOf c
                      }
                )
                children
        }

summariseCoveragePhase :: CoveragePhaseTiming -> PhaseTimingSummary
summariseCoveragePhase CoveragePhaseTiming {..} =
  let children = coveragePhaseTimingChildren
      costsOf c = childCosts (coverageChildTimingWallNanos c) (coverageChildTimingInner c)
      wallOf = coverageChildTimingWallNanos
      retried = length [c | c <- children, coverageChildTimingAttempts c > 1]
      entryLabel c =
        T.concat
          [ renderTestId (coverageChildTimingTestId c),
            " (",
            coverageChildTimingSuite c,
            ", ",
            T.pack (show (coverageChildTimingMutationsCovered c)),
            " mutations",
            if coverageChildTimingAttempts c > 1
              then T.concat [", ", T.pack (show (coverageChildTimingAttempts c)), " attempts"]
              else "",
            ")"
          ]
   in PhaseTimingSummary
        { phaseTimingSummaryLabel = "coverage phase",
          phaseTimingSummaryWallNanos = coveragePhaseTimingWallNanos,
          phaseTimingSummaryJobs = coveragePhaseTimingJobs,
          phaseTimingSummaryChildren = fromIntegral (length children),
          phaseTimingSummaryChildWallNanos = sum (map wallOf children),
          phaseTimingSummaryCosts = foldMap costsOf children,
          phaseTimingSummaryNotes =
            [("Tests that needed a retry", T.pack (show retried)) | retried > 0],
          phaseTimingSummaryDimensions =
            [ TimingDimension "suite" (bucketBy coverageChildTimingSuite wallOf costsOf children)
            ],
          phaseTimingSummaryEntries =
            sortOn (Down . timingEntryWallNanos) $
              map
                ( \c ->
                    TimingEntry
                      { timingEntryLabel = entryLabel c,
                        timingEntryCells =
                          [ ("test", renderTestId (coverageChildTimingTestId c)),
                            ("suite", coverageChildTimingSuite c),
                            ( "mutations covered",
                              T.pack (show (coverageChildTimingMutationsCovered c))
                            ),
                            ("attempts", T.pack (show (coverageChildTimingAttempts c)))
                          ],
                        timingEntryWallNanos = wallOf c,
                        timingEntryCosts = costsOf c
                      }
                )
                children
        }

-- | Render a summary for a terminal.
--
-- @limit@ caps how many rows each grouped view and the per-child listing
-- show, with a line saying what was left out and where the whole thing is: a
-- run with thousands of mutations would otherwise bury the rest of the build
-- log, and the full detail belongs in @timing.html@ anyway, which can sort
-- and filter it.
renderPhaseTimingSummary :: Int -> PhaseTimingSummary -> [[Chunk]]
renderPhaseTimingSummary limit PhaseTimingSummary {..} =
  concat
    [ [[fore blue (chunk (T.concat ["Timing report: ", phaseTimingSummaryLabel]))], []],
      map
        (\(l, v) -> [chunk "  ", chunk (T.justifyLeft 26 ' ' l), chunk (T.justifyRight 12 ' ' v)])
        ( [ ("Wall clock", renderDurationNanos phaseTimingSummaryWallNanos),
            ("Children", T.pack (show phaseTimingSummaryChildren)),
            ("Sum of child wall time", renderDurationNanos phaseTimingSummaryChildWallNanos),
            ( "Achieved parallelism",
              T.concat
                [ T.pack (printf "%.2fx" parallelism),
                  " of ",
                  T.pack (show phaseTimingSummaryJobs)
                ]
            )
          ]
            ++ phaseTimingSummaryNotes
        ),
      [[], [chunk "Where the child time goes  ", fore cyan (chunk legend)]],
      costLines phaseTimingSummaryCosts,
      concatMap renderDimension phaseTimingSummaryDimensions,
      [[], [chunk "Per child, slowest first  ", fore cyan (chunk "(wall, bar, then startup/setup/tests)")]],
      limited (map renderEntry phaseTimingSummaryEntries) (length phaseTimingSummaryEntries) ("child", "children")
    ]
  where
    legend :: Text
    legend =
      T.concat
        [ "(",
          processChar,
          " startup  ",
          setupChar,
          " setup  ",
          testChar,
          " tests  ",
          unattributedChar,
          " unattributed)"
        ]

    totalChildNanos = phaseTimingSummaryChildWallNanos
    parallelism :: Double
    parallelism =
      if phaseTimingSummaryWallNanos == 0
        then 0
        else fromIntegral totalChildNanos / fromIntegral phaseTimingSummaryWallNanos

    -- Every share and every bar in the report is relative to the summed child
    -- wall time, so a bar in one section is directly comparable to a bar in
    -- another.
    share :: Word64 -> Double
    share n = if totalChildNanos == 0 then 0 else fromIntegral n / fromIntegral totalChildNanos

    bar :: CostBreakdown -> Text
    bar = renderStackedBar barWidth totalChildNanos

    limited rendered total (singularNoun, pluralNoun)
      | total <= limit = rendered
      | otherwise =
          let omitted = total - limit
           in take limit rendered
                ++ [ [ fore
                         cyan
                         ( chunk
                             ( T.concat
                                 [ "    ... and ",
                                   T.pack (show omitted),
                                   " more ",
                                   if omitted == 1 then singularNoun else pluralNoun,
                                   "; timing.html has the full listing"
                                 ]
                             )
                         )
                     ]
                   ]

    -- One row per cost, each drawn from a breakdown holding only that cost,
    -- so the row's bar uses that cost's own shade.
    costLines cb@CostBreakdown {..} =
      let row :: Colour -> Text -> CostBreakdown -> [Chunk]
          row colour label onlyCost =
            let n = costTotalNanos onlyCost
             in [ chunk "  ",
                  chunk (T.justifyLeft 18 ' ' label),
                  fore colour (chunk (T.justifyRight 12 ' ' (renderDurationNanos n))),
                  chunk (T.justifyRight 8 ' ' (percentText (share n))),
                  chunk "  ",
                  -- Nothing follows this bar, so it is not padded to the
                  -- column width: trailing spaces would be the end of every
                  -- one of these lines in the written report.
                  fore colour (chunk (T.stripEnd (bar onlyCost)))
                ]
       in [ row red "process startup" mempty {costProcessNanos = costProcessNanos},
            row yellow "suite setup" mempty {costSetupNanos = costSetupNanos},
            row green "test execution" mempty {costTestNanos = costTestNanos}
          ]
            ++ [ row blue "unattributed" mempty {costUnattributedNanos = costUnattributedNanos}
               | costUnattributedNanos > 0
               ]
            ++ [ [ chunk "  ",
                   fore cyan (chunk "unattributed is children that reported no breakdown, such as one killed on its timeout")
                 ]
               | costUnattributedNanos > 0
               ]
            ++ [ [ chunk "  ",
                   chunk (T.justifyLeft 18 ' ' "total"),
                   chunk (T.justifyRight 12 ' ' (renderDurationNanos (costTotalNanos cb)))
                 ]
               ]

    renderDimension TimingDimension {timingDimensionName, timingDimensionBuckets} =
      concat
        [ [[], [chunk (T.concat ["By ", timingDimensionName])]],
          limited
            (map renderBucket timingDimensionBuckets)
            (length timingDimensionBuckets)
            (timingDimensionName, timingDimensionName <> "s")
        ]

    renderBucket TimingBucket {..} =
      [ chunk "  ",
        chunk (T.justifyRight 6 ' ' (T.pack (show timingBucketChildren))),
        chunk (T.justifyRight 12 ' ' (renderDurationNanos timingBucketWallNanos)),
        chunk (T.justifyRight 8 ' ' (percentText (share timingBucketWallNanos))),
        chunk "  ",
        chunk (bar timingBucketCosts),
        chunk "  ",
        chunk timingBucketLabel
      ]

    renderEntry TimingEntry {..} =
      [ chunk "  ",
        chunk (T.justifyRight 12 ' ' (renderDurationNanos timingEntryWallNanos)),
        chunk "  ",
        chunk (bar timingEntryCosts),
        chunk "  ",
        chunk (T.justifyRight 26 ' ' (splitText timingEntryCosts)),
        chunk "  ",
        chunk timingEntryLabel
      ]

    -- The three costs as one compact column, so the per-child listing stays
    -- one line per child while still saying which cost dominates it.  A child
    -- that reported no breakdown would otherwise read as three genuine zeroes.
    splitText CostBreakdown {..}
      | costUnattributedNanos > 0 = "unattributed"
      | otherwise =
          T.intercalate
            "/"
            [ renderDurationNanos costProcessNanos,
              renderDurationNanos costSetupNanos,
              renderDurationNanos costTestNanos
            ]

percentText :: Double -> Text
percentText f = T.pack (printf "%.1f%%" (100 * f))

-- | How wide every bar in a rendered report is.
barWidth :: Int
barWidth = 24

processChar, setupChar, testChar, unattributedChar :: Text
processChar = "\9619"
setupChar = "\9618"
testChar = "\9608"
unattributedChar = "\9617"

-- | A horizontal bar for one 'CostBreakdown', as a fraction of @total@,
-- padded to @width@ cells.
--
-- The bar's length is the breakdown's share of the total and its segments are
-- the three costs, each drawn in its own shade, so one row shows both how
-- much time went here and what that time was spent on.  A cost too small for
-- a whole cell is dropped rather than rounded up, so segment lengths stay
-- proportional and the bar never overflows its column.
renderStackedBar :: Int -> Word64 -> CostBreakdown -> Text
renderStackedBar width total CostBreakdown {..} =
  let cellAt :: Word64 -> Int
      cellAt n =
        if total == 0
          then 0
          else round (fromIntegral width * fromIntegral n / (fromIntegral total :: Double))
      -- Cell positions of the segment boundaries, so rounding cannot make the
      -- segments sum to more than the bar.
      boundaries =
        map cellAt $
          scanl
            (+)
            0
            [costProcessNanos, costSetupNanos, costTestNanos, costUnattributedNanos]
      segment c from to = T.replicate (max 0 (to - from)) c
      bar =
        T.concat $
          zipWith3
            segment
            [processChar, setupChar, testChar, unattributedChar]
            boundaries
            (drop 1 boundaries)
   in -- Truncate as well as pad: the cumulative boundaries are computed on
      -- 'Word64' sums, which a pathological breakdown can overflow into a
      -- non-monotonic sequence, and a bar wider than its column would break
      -- every row after it.
      T.justifyLeft width ' ' (T.take width bar)

-- | A duration in a unit a human can compare at a glance, widest unit first.
renderDurationNanos :: Word64 -> Text
renderDurationNanos nanos =
  let seconds :: Double
      seconds = fromIntegral nanos / 1e9
      wholeSeconds :: Word64
      wholeSeconds = nanos `div` 1_000_000_000
      hours = wholeSeconds `div` 3600
      minutes = (wholeSeconds `mod` 3600) `div` 60
   in T.pack $
        if
          | wholeSeconds >= 3600 -> printf "%dh%02dm%02ds" hours minutes (wholeSeconds `mod` 60)
          | wholeSeconds >= 60 -> printf "%dm%02ds" minutes (wholeSeconds `mod` 60)
          | seconds >= 1 -> printf "%.2fs" seconds
          | nanos >= 1_000_000 -> printf "%.1fms" (fromIntegral nanos / 1e6 :: Double)
          | nanos >= 1_000 -> printf "%.1fus" (fromIntegral nanos / 1e3 :: Double)
          | otherwise -> printf "%dns" nanos
