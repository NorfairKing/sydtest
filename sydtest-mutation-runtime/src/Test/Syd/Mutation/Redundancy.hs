{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Redundant-test analysis for mutation testing.
--
-- Given a relation @R(t)@ assigning to each test the set of mutations it is
-- associated with — either the mutations it /reaches/ (coverage basis) or the
-- mutations it actually /catches/ (kill basis) — 'analyzeRedundancy' produces a
-- 'RedundancyReport' describing which tests are redundant under three
-- complementary lenses (equivalence classes, subsumption, and a greedy minimal
-- suite), plus the load-bearing sole-killer tests and the dual
-- redundant-mutant grouping.
module Test.Syd.Mutation.Redundancy
  ( RedundancyBasis (..),
    Subsumption (..),
    RedundancyReport (..),
    analyzeRedundancy,
    writeRedundancyReportFile,
    readRedundancyReportFile,
    readRedundancyReportFileIfExists,
    RedundancyReportDecodeException (..),
  )
where

import Autodocodec
import Control.Exception (Exception, throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Data.List (sort, sortBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Path
import Path.IO (ensureDir, forgivingAbsence)
import Test.Syd.Mutation.Runtime (MutationId)
import Test.Syd.Mutation.TestId (TestId)

-- | Which relation the redundancy analysis was computed over.
--
-- 'BasisCoverage' uses the mutations each test /reaches/: cheap (pure
-- post-processing of cached coverage) but approximate — a test flagged
-- redundant here might still be the only one whose assertion actually
-- /catches/ a mutation it reaches.  'BasisKill' uses the mutations each test
-- actually catches: accurate, but requires running the tests against the
-- mutants to observe per-test pass\/fail.
data RedundancyBasis = BasisCoverage | BasisKill
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec RedundancyBasis)

instance Validity RedundancyBasis

instance GenValid RedundancyBasis where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec RedundancyBasis where
  codec = bimapCodec dec enc codec
    where
      enc :: RedundancyBasis -> Text
      enc = \case
        BasisCoverage -> "coverage"
        BasisKill -> "kill"
      dec :: Text -> Either String RedundancyBasis
      dec = \case
        "coverage" -> Right BasisCoverage
        "kill" -> Right BasisKill
        other -> Left ("unknown redundancy basis: " <> T.unpack other)

-- | One strict-subsumption edge: 'subsumptionDominated' is a test whose
-- associated mutation set is a strict subset of 'subsumptionDominator'\'s, so
-- the dominator catches everything the dominated test does and more.
--
-- Equal sets are reported as equivalence classes (see
-- 'redundancyReportEquivClasses'), not as subsumptions, so
-- 'subsumptionStrict' is always 'True' for values produced by
-- 'analyzeRedundancy'; the field is kept for forward compatibility and
-- round-trip clarity.
data Subsumption = Subsumption
  { subsumptionDominated :: TestId,
    subsumptionDominator :: TestId,
    subsumptionStrict :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec Subsumption)

instance Validity Subsumption

instance GenValid Subsumption where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance HasCodec Subsumption where
  codec =
    object "Subsumption" $
      Subsumption
        <$> requiredField' "dominated" .= subsumptionDominated
        <*> requiredField' "dominator" .= subsumptionDominator
        <*> optionalFieldWithDefault' "strict" True .= subsumptionStrict

-- | The result of a redundancy analysis over one test suite.
--
-- All lenses are restricted to the mutations actually caught (the input
-- relation is expected to be restricted to killed mutants by the caller):
-- mutations that no test catches are irrelevant to redundancy.
data RedundancyReport = RedundancyReport
  { -- | Which relation this was computed over.
    redundancyReportBasis :: RedundancyBasis,
    -- | Suite name; @""@ for an anonymous\/single-suite setup.
    redundancyReportSuite :: Text,
    -- | Groups of tests with /identical/ associated mutation sets (each group
    -- has size ≥ 2 and a non-empty set).  Keep any one member of each group;
    -- the rest are redundant.  Members are ordered fastest-first.
    redundancyReportEquivClasses :: [[TestId]],
    -- | Strict-subsumption edges: each dominated test is strictly contained in
    -- its dominator.
    redundancyReportSubsumptions :: [Subsumption],
    -- | A greedy minimum-set-cover of the caught mutations: a reduced suite
    -- that still catches every mutation the full suite catches.  The cover is
    -- not unique.
    redundancyReportMinimalSuite :: [TestId],
    -- | The complement of 'redundancyReportMinimalSuite': tests the greedy
    -- cover does not need.
    redundancyReportRemovable :: [TestId],
    -- | Load-bearing tests: each is the /sole/ test catching at least one
    -- mutation, listed with those mutations.  Never redundant.
    redundancyReportSoleKillers :: Map.Map TestId [MutationId],
    -- | Dual grouping: sets of mutations caught by an identical set of tests
    -- (each group has size ≥ 2).  Signals over-generating mutation operators.
    redundancyReportRedundantMutants :: [[MutationId]]
  }
  deriving stock (Show, Eq, Generic)
  deriving (Aeson.ToJSON, Aeson.FromJSON) via (Autodocodec RedundancyReport)

instance Validity RedundancyReport

instance GenValid RedundancyReport where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

-- | Codec for @'Map' 'TestId' ['MutationId']@: a JSON array of
-- @{test, mutations}@ objects, because a 'TestId' is not a valid JSON object
-- key (mirrors 'Test.Syd.Mutation.TestCoverageMap').
soleKillersCodec :: JSONCodec (Map.Map TestId [MutationId])
soleKillersCodec =
  dimapCodec
    Map.fromList
    Map.toList
    ( listCodec $
        object "SoleKiller" $
          (,)
            <$> requiredField' "test" .= fst
            <*> requiredField' "mutations" .= snd
    )

instance HasCodec RedundancyReport where
  codec =
    object "RedundancyReport" $
      RedundancyReport
        <$> requiredField' "basis" .= redundancyReportBasis
        <*> optionalFieldWithDefault' "suite" "" .= redundancyReportSuite
        <*> optionalFieldWithDefault' "equivalence_classes" [] .= redundancyReportEquivClasses
        <*> optionalFieldWithDefault' "subsumptions" [] .= redundancyReportSubsumptions
        <*> optionalFieldWithDefault' "minimal_suite" [] .= redundancyReportMinimalSuite
        <*> optionalFieldWithDefault' "removable" [] .= redundancyReportRemovable
        <*> optionalFieldWithDefaultWith' "sole_killers" soleKillersCodec Map.empty .= redundancyReportSoleKillers
        <*> optionalFieldWithDefault' "redundant_mutants" [] .= redundancyReportRedundantMutants

-- | Analyse the redundancy of a suite's tests.
--
-- The input @rel@ maps every known test to the set of mutations it is
-- associated with under @basis@, restricted to killed mutants.  Tests that are
-- associated with no killed mutant must still be present (mapped to the empty
-- set) so they can be reported as removable.  @baselines@ gives per-test
-- monotonic-clock baselines used only to break ties (fastest test wins) so the
-- output is deterministic and the suggested representatives are cheap; a test
-- with no recorded baseline sorts last.
analyzeRedundancy ::
  RedundancyBasis ->
  Text ->
  Map.Map TestId Word ->
  Map.Map TestId (Set MutationId) ->
  RedundancyReport
analyzeRedundancy basis suite baselines rel
  | Set.null universe =
      -- No mutation is caught by any test: redundancy is vacuous.  Report
      -- nothing rather than declaring every test removable.
      RedundancyReport
        { redundancyReportBasis = basis,
          redundancyReportSuite = suite,
          redundancyReportEquivClasses = [],
          redundancyReportSubsumptions = [],
          redundancyReportMinimalSuite = [],
          redundancyReportRemovable = [],
          redundancyReportSoleKillers = Map.empty,
          redundancyReportRedundantMutants = []
        }
  | otherwise =
      RedundancyReport
        { redundancyReportBasis = basis,
          redundancyReportSuite = suite,
          redundancyReportEquivClasses = equivClasses,
          redundancyReportSubsumptions = subsumptions,
          redundancyReportMinimalSuite = minimalSuite,
          redundancyReportRemovable = removable,
          redundancyReportSoleKillers = soleKillers,
          redundancyReportRedundantMutants = redundantMutants
        }
  where
    tests :: [TestId]
    tests = Map.keys rel

    r :: TestId -> Set MutationId
    r t = Map.findWithDefault Set.empty t rel

    universe :: Set MutationId
    universe = Set.unions (Map.elems rel)

    -- Tie-break key: fastest first, then by 'TestId' for determinism.  A test
    -- with no recorded baseline sorts last.
    keyOf :: TestId -> (Word, TestId)
    keyOf t = (Map.findWithDefault maxBound t baselines, t)

    -- Inverse relation: which tests catch each mutation.
    killersOf :: Map.Map MutationId (Set TestId)
    killersOf =
      Map.fromListWith
        Set.union
        [ (m, Set.singleton t)
        | (t, ms) <- Map.toList rel,
          m <- Set.toList ms
        ]

    equivClasses :: [[TestId]]
    equivClasses =
      [ sortOn keyOf grp
      | grp <- Map.elems byValue,
        length grp >= 2
      ]
      where
        byValue =
          Map.fromListWith
            (++)
            [ (r t, [t])
            | t <- tests,
              not (Set.null (r t))
            ]

    subsumptions :: [Subsumption]
    subsumptions =
      [ Subsumption
          { subsumptionDominated = t,
            subsumptionDominator = dom,
            subsumptionStrict = True
          }
      | t <- sortOn keyOf tests,
        not (Set.null (r t)),
        dom <- maybeToList (bestDominator t)
      ]
      where
        bestDominator t =
          listToMaybe $
            sortBy (comparing (Down . Set.size . r) <> comparing keyOf) $
              [ t'
              | t' <- tests,
                t' /= t,
                r t `Set.isSubsetOf` r t',
                r t /= r t'
              ]

    minimalSuite :: [TestId]
    minimalSuite = sortOn keyOf (greedy universe [])
      where
        greedy uncovered chosen
          | Set.null uncovered = chosen
          | otherwise = case bestPick uncovered of
              Nothing -> chosen
              Just t -> greedy (uncovered Set.\\ r t) (t : chosen)
        bestPick uncovered =
          fmap snd $
            listToMaybe $
              sortBy (comparing (Down . fst) <> comparing (keyOf . snd)) $
                [ (gain, t)
                | t <- tests,
                  let gain = Set.size (Set.intersection (r t) uncovered),
                  gain > 0
                ]

    removable :: [TestId]
    removable = sortOn keyOf (filter (`Set.notMember` chosenSet) tests)
      where
        chosenSet = Set.fromList minimalSuite

    soleKillers :: Map.Map TestId [MutationId]
    soleKillers =
      Map.map sort $
        Map.fromListWith
          (++)
          [ (t, [m])
          | (m, ts) <- Map.toList killersOf,
            [t] <- [Set.toList ts]
          ]

    redundantMutants :: [[MutationId]]
    redundantMutants =
      [ sort grp
      | grp <- Map.elems byKillers,
        length grp >= 2
      ]
      where
        byKillers =
          Map.fromListWith
            (++)
            [ (ts, [m])
            | (m, ts) <- Map.toList killersOf,
              not (Set.null ts)
            ]

redundancyReportRelFile :: Path Rel File
redundancyReportRelFile = [relfile|redundancy.json|]

-- | Write @redundancy.json@ to the given directory.
writeRedundancyReportFile :: Path Abs Dir -> RedundancyReport -> IO ()
writeRedundancyReportFile dir report = do
  ensureDir dir
  LB.writeFile (fromAbsFile (dir </> redundancyReportRelFile)) (Aeson.encode report)

-- | Thrown by 'readRedundancyReportFile' when @redundancy.json@ cannot be
-- decoded.
newtype RedundancyReportDecodeException
  = RedundancyReportDecodeException FilePath
  deriving (Show)

instance Exception RedundancyReportDecodeException

-- | Read @redundancy.json@ from the given directory.  Reads strictly so the
-- file handle is closed before returning.
readRedundancyReportFile :: Path Abs Dir -> IO RedundancyReport
readRedundancyReportFile dir = do
  let path = dir </> redundancyReportRelFile
  result <- Aeson.decodeStrict <$> B.readFile (fromAbsFile path)
  case result of
    Nothing -> throwIO (RedundancyReportDecodeException (fromAbsFile path))
    Just m -> pure m

-- | Read @redundancy.json@, returning 'Nothing' if the file does not exist.
readRedundancyReportFileIfExists :: Path Abs Dir -> IO (Maybe RedundancyReport)
readRedundancyReportFileIfExists dir =
  forgivingAbsence (readRedundancyReportFile dir)
