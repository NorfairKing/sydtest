{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Driver.CoverageSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest (AugmentedMutationRecord (..))
import Test.Syd.Mutation.Driver.Coverage (annotateRecord, controlCoveringTestLimit)
import Test.Syd.Mutation.Manifest (MutationRecord (..), controlOperatorName)
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId (..))

spec :: Spec
spec = describe "annotateRecord" $ do
  it "keeps every covering test of an ordinary mutation" $ do
    let tests = [TestId ((name, 0) :| []) | name <- ["a", "b", "c"]]
    let record =
          MutationRecord
            { mutRecId = MutationId ["M", "Op", "1", "1", "2"],
              mutRecOperator = "Op",
              mutRecOriginal = "+",
              mutRecReplacement = "-",
              mutRecModule = "M",
              mutRecLine = 1,
              mutRecEndLine = 1,
              mutRecColStart = 1,
              mutRecColEnd = 2,
              mutRecSourceFile = Nothing,
              mutRecSourceLines = [],
              mutRecMutatedLines = [],
              mutRecContextBefore = [],
              mutRecContextAfter = [],
              mutRecCoveringTests = Nothing,
              mutRecBinding = Nothing,
              mutRecMitigation = Nothing
            }
    let annotated =
          annotateRecord
            "suite"
            (Map.singleton (mutRecId record) (Set.fromList tests))
            (Map.fromList (zip tests [1_000_000, 2_000_000, 3_000_000]))
            record
    map augmentedMutationRecordCoveringTests annotated
      `shouldBe` [Map.singleton "suite" tests]
    map augmentedMutationRecordTimeoutMicros annotated
      `shouldBe` [60_000_000]

  it "keeps a bounded sample of a control's covering tests" $ do
    -- A control runs every covering test it is given rather than stopping at
    -- the one that kills the mutation, so what it is given is capped.  Which
    -- ones follow from the mutation's id, so a rerun of the same coverage
    -- picks the same tests; the budget follows them because it is derived
    -- from them.
    let tests =
          [ TestId ((T.pack ("test-" ++ show i), 0) :| [])
          | i <- [1 .. controlCoveringTestLimit + 2]
          ]
    let record =
          MutationRecord
            { mutRecId = MutationId ["M", "Control", "1", "1", "2", "no-op", "0"],
              mutRecOperator = controlOperatorName,
              mutRecOriginal = "no-op",
              mutRecReplacement = "no-op",
              mutRecModule = "M",
              mutRecLine = 1,
              mutRecEndLine = 1,
              mutRecColStart = 1,
              mutRecColEnd = 2,
              mutRecSourceFile = Nothing,
              mutRecSourceLines = [],
              mutRecMutatedLines = [],
              mutRecContextBefore = [],
              mutRecContextAfter = [],
              mutRecCoveringTests = Nothing,
              mutRecBinding = Nothing,
              mutRecMitigation = Nothing
            }
    let annotate =
          annotateRecord
            "suite"
            (Map.singleton (mutRecId record) (Set.fromList tests))
            (Map.fromList [(t, 1_000_000) | t <- tests])
            record
    let kept = concatMap (Map.findWithDefault [] "suite" . augmentedMutationRecordCoveringTests) annotate
    length kept `shouldBe` controlCoveringTestLimit
    Set.difference (Set.fromList kept) (Set.fromList tests) `shouldBe` Set.empty
    -- Ten times the cost of the ten it kept, rather than of all twelve.
    map augmentedMutationRecordTimeoutMicros annotate
      `shouldBe` [fromIntegral controlCoveringTestLimit * 10_000_000]

  it "keeps every covering test of a control that has fewer than the cap" $ do
    let tests = [TestId ((T.pack ("test-" ++ show i), 0) :| []) | i <- [1 .. 3 :: Int]]
    let record =
          MutationRecord
            { mutRecId = MutationId ["M", "Control", "1", "1", "2", "no-op", "0"],
              mutRecOperator = controlOperatorName,
              mutRecOriginal = "no-op",
              mutRecReplacement = "no-op",
              mutRecModule = "M",
              mutRecLine = 1,
              mutRecEndLine = 1,
              mutRecColStart = 1,
              mutRecColEnd = 2,
              mutRecSourceFile = Nothing,
              mutRecSourceLines = [],
              mutRecMutatedLines = [],
              mutRecContextBefore = [],
              mutRecContextAfter = [],
              mutRecCoveringTests = Nothing,
              mutRecBinding = Nothing,
              mutRecMitigation = Nothing
            }
    let annotated =
          annotateRecord
            "suite"
            (Map.singleton (mutRecId record) (Set.fromList tests))
            Map.empty
            record
    map (Set.fromList . Map.findWithDefault [] "suite" . augmentedMutationRecordCoveringTests) annotated
      `shouldBe` [Set.fromList tests]
