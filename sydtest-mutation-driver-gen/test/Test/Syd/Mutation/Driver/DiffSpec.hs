{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.Mutation.Driver.DiffSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
  )
import Test.Syd.Mutation.Driver.Diff
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.Mutation.TestId (TestId, parseTestIdFilterArg)

-- | Build an augmented record with the fields the diff selection looks at;
-- the rest are filler.
mkRecord ::
  -- | id
  [String] ->
  -- | source file (package-relative)
  Maybe (Path Rel File) ->
  -- | (line, end_line)
  (Word, Word) ->
  -- | covering tests, by suite
  Map.Map Text [TestId] ->
  AugmentedMutationRecord
mkRecord idParts mSrc (line, endLine) covering =
  AugmentedMutationRecord
    { augmentedMutationRecordId = MutationId idParts,
      augmentedMutationRecordOperator = "Op",
      augmentedMutationRecordOriginal = "+",
      augmentedMutationRecordReplacement = "-",
      augmentedMutationRecordModule = "M",
      augmentedMutationRecordLine = line,
      augmentedMutationRecordEndLine = endLine,
      augmentedMutationRecordColStart = 1,
      augmentedMutationRecordColEnd = 2,
      augmentedMutationRecordSourceFile = mSrc,
      augmentedMutationRecordSourceLines = [],
      augmentedMutationRecordMutatedLines = [],
      augmentedMutationRecordContextBefore = [],
      augmentedMutationRecordContextAfter = [],
      augmentedMutationRecordCoveringTests = covering,
      augmentedMutationRecordTimeoutMicros = 30000000,
      augmentedMutationRecordBinding = Nothing,
      augmentedMutationRecordMitigation = Nothing
    }

tid :: Text -> TestId
tid t = case parseTestIdFilterArg t of
  Just i -> i
  Nothing -> error ("DiffSpec: unparseable test id in test fixture: " ++ T.unpack t)

-- | A hunk over @[s..e]@ where the whole span is also the added range — the
-- common "all lines in this range were added" fixture.
addedHunk :: Path Rel File -> Word -> Word -> DiffHunk
addedHunk file s e =
  DiffHunk
    { diffHunkFile = file,
      diffHunkSpanStart = s,
      diffHunkSpanEnd = e,
      diffHunkAddedRanges = [(s, e)]
    }

spec :: Spec
spec = do
  describe "parseUnifiedDiff" $ do
    it "parses a single-file single-hunk added run" $ do
      let d =
            T.unlines
              [ "diff --git a/sydtest-mutation-example/src/Example/Lib.hs b/sydtest-mutation-example/src/Example/Lib.hs",
                "index abc..def 100644",
                "--- a/sydtest-mutation-example/src/Example/Lib.hs",
                "+++ b/sydtest-mutation-example/src/Example/Lib.hs",
                "@@ -8,3 +8,4 @@ addOne x =",
                " context line",
                "+added line one",
                "+added line two",
                " more context"
              ]
      parseUnifiedDiff d
        `shouldBe` Right
          [ DiffHunk
              { diffHunkFile = [relfile|sydtest-mutation-example/src/Example/Lib.hs|],
                diffHunkSpanStart = 8,
                diffHunkSpanEnd = 11,
                diffHunkAddedRanges = [(9, 10)]
              }
          ]

    it "records separate added runs within one hunk" $ do
      let d =
            T.unlines
              [ "+++ b/A.hs",
                "@@ -1,5 +1,7 @@",
                " a",
                "+b",
                " c",
                "+d",
                "+e",
                " f"
              ]
      parseUnifiedDiff d
        `shouldBe` Right
          [ DiffHunk
              { diffHunkFile = [relfile|A.hs|],
                -- 6 new-side lines in the body: a,b,c,d,e,f.
                diffHunkSpanStart = 1,
                diffHunkSpanEnd = 6,
                diffHunkAddedRanges = [(2, 2), (4, 5)]
              }
          ]

    it "advances new-side line numbers past context but not deletions" $ do
      let d =
            T.unlines
              [ "+++ b/A.hs",
                "@@ -1,4 +1,4 @@",
                " a",
                "-removed",
                "+replacement",
                " b"
              ]
      -- new-side: line 1 = 'a' (context), line 2 = 'replacement' (added),
      -- line 3 = 'b' (context).  The deletion does not advance the counter.
      parseUnifiedDiff d
        `shouldBe` Right
          [ DiffHunk
              { diffHunkFile = [relfile|A.hs|],
                diffHunkSpanStart = 1,
                diffHunkSpanEnd = 3,
                diffHunkAddedRanges = [(2, 2)]
              }
          ]

    it "skips /dev/null new-side files (deletions)" $ do
      let d =
            T.unlines
              [ "+++ /dev/null",
                "@@ -1,2 +0,0 @@",
                "-gone one",
                "-gone two"
              ]
      parseUnifiedDiff d `shouldBe` Right []

  describe "pathMatchesHunkFile" $ do
    it "matches when the package-relative path is a component suffix of the diff path" $
      pathMatchesHunkFile
        [relfile|src/Example/Lib.hs|]
        [relfile|sydtest-mutation-example/src/Example/Lib.hs|]
        `shouldBe` True
    it "does not match on a partial last-component overlap" $
      pathMatchesHunkFile
        [relfile|b/Lib.hs|]
        [relfile|ab/Lib.hs|]
        `shouldBe` False
    it "matches an exact equal path" $
      pathMatchesHunkFile [relfile|src/A.hs|] [relfile|src/A.hs|] `shouldBe` True

  describe "mutationsInHunks" $ do
    let recA = mkRecord ["M", "Op", "10"] (Just [relfile|src/A.hs|]) (10, 10) Map.empty
        recB = mkRecord ["M", "Op", "20"] (Just [relfile|src/A.hs|]) (20, 22) Map.empty
        recOther = mkRecord ["N", "Op", "5"] (Just [relfile|src/B.hs|]) (5, 5) Map.empty
        manifest = AugmentedManifest [AugmentedMutationGroup [recA, recB, recOther]]
    it "selects only mutations whose span intersects a hunk in a matching file" $ do
      let hunks = [addedHunk [relfile|pkg/src/A.hs|] 9 11]
      mutationsInHunks hunks manifest
        `shouldBe` Set.singleton (MutationId ["M", "Op", "10"])
    it "selects a multi-line span when the hunk overlaps its end" $ do
      let hunks = [addedHunk [relfile|pkg/src/A.hs|] 21 21]
      mutationsInHunks hunks manifest
        `shouldBe` Set.singleton (MutationId ["M", "Op", "20"])
    it "selects nothing when the hunk is in an unrelated file" $
      mutationsInHunks [addedHunk [relfile|pkg/src/C.hs|] 1 100] manifest
        `shouldBe` Set.empty

  describe "testsInHunks" $ do
    let locs =
          Map.fromList
            [ (tid "Spec.one", ([relfile|test/Spec.hs|], 5)),
              (tid "Spec.two", ([relfile|test/Spec.hs|], 9))
            ]
    it "selects tests whose source line falls in a hunk" $
      testsInHunks [addedHunk [relfile|pkg/test/Spec.hs|] 8 10] locs
        `shouldBe` Set.singleton (tid "Spec.two")

  describe "selectMutations" $
    it "unions source-selected and test-covered mutations" $ do
      let srcRec = mkRecord ["M", "Op", "10"] (Just [relfile|src/A.hs|]) (10, 10) Map.empty
          covRec =
            mkRecord
              ["M", "Op", "99"]
              (Just [relfile|src/A.hs|])
              (99, 99)
              (Map.singleton "" [tid "Spec.two"])
          manifest = AugmentedManifest [AugmentedMutationGroup [srcRec, covRec]]
          locs = Map.singleton "" (Map.fromList [(tid "Spec.two", ([relfile|test/Spec.hs|], 9))])
          -- One hunk touches the source line 10; another touches the test
          -- line 9 (whose test covers mutation 99).
          hunks =
            [ addedHunk [relfile|pkg/src/A.hs|] 10 10,
              addedHunk [relfile|pkg/test/Spec.hs|] 9 9
            ]
      selectMutations hunks manifest locs
        `shouldBe` Set.fromList
          [ MutationId ["M", "Op", "10"],
            MutationId ["M", "Op", "99"]
          ]
