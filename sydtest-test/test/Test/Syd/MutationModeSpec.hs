{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Syd.MutationModeSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Path
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest (AugmentedMutationRecord (..))
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.MutationMode (formatMutationLog, renderUnifiedDiff)
import Text.Colour (TerminalCapabilities (..), renderChunksText)

spec :: Spec
spec = do
  describe "formatMutationLog" $ do
    -- The function pattern-matches on the parts list length: <5 falls back
    -- to the slash-joined id, 5+ formats a header with source location,
    -- and exactly 7 appends an alt-index suffix.  These tests pin down
    -- the boundary behaviour at each length so a future change cannot
    -- silently move a case into a different branch.
    describe "fallback (fewer than 5 parts)" $ do
      it "shows the mutation id parts joined by slashes (2 parts)" $ do
        let aRecord =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"],
                  augmentedMutationRecordOperator = "ArithOp",
                  augmentedMutationRecordOriginal = "(+)",
                  augmentedMutationRecordReplacement = "(-)",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 42,
                  augmentedMutationRecordEndLine = 42,
                  augmentedMutationRecordColStart = 10,
                  augmentedMutationRecordColEnd = 12,
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = [],
                  augmentedMutationRecordCoveringTests = Map.empty,
                  augmentedMutationRecordTimeoutMicros = 30000000
                }
            rendered = unlines (map (T.unpack . renderChunksText WithoutColours) (formatMutationLog (MutationId ["only-two", "parts"]) aRecord))
        rendered `shouldBe` "only-two/parts\n"
      it "shows the mutation id parts joined by slashes (3 parts)" $ do
        let aRecord =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"],
                  augmentedMutationRecordOperator = "ArithOp",
                  augmentedMutationRecordOriginal = "(+)",
                  augmentedMutationRecordReplacement = "(-)",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 42,
                  augmentedMutationRecordEndLine = 42,
                  augmentedMutationRecordColStart = 10,
                  augmentedMutationRecordColEnd = 12,
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = [],
                  augmentedMutationRecordCoveringTests = Map.empty,
                  augmentedMutationRecordTimeoutMicros = 30000000
                }
            rendered = unlines (map (T.unpack . renderChunksText WithoutColours) (formatMutationLog (MutationId ["a", "b", "c"]) aRecord))
        rendered `shouldBe` "a/b/c\n"
      it "shows the mutation id parts joined by slashes (4 parts)" $ do
        let aRecord =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"],
                  augmentedMutationRecordOperator = "ArithOp",
                  augmentedMutationRecordOriginal = "(+)",
                  augmentedMutationRecordReplacement = "(-)",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 42,
                  augmentedMutationRecordEndLine = 42,
                  augmentedMutationRecordColStart = 10,
                  augmentedMutationRecordColEnd = 12,
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = [],
                  augmentedMutationRecordCoveringTests = Map.empty,
                  augmentedMutationRecordTimeoutMicros = 30000000
                }
            rendered = unlines (map (T.unpack . renderChunksText WithoutColours) (formatMutationLog (MutationId ["a", "b", "c", "d"]) aRecord))
        rendered `shouldBe` "a/b/c/d\n"

    describe "with 6 parts (one extra, no alt-index disambiguation)" $
      it "renders the header without an alt-index suffix" $ do
        let record =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"],
                  augmentedMutationRecordOperator = "ArithOp",
                  augmentedMutationRecordOriginal = "(+)",
                  augmentedMutationRecordReplacement = "(-)",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 42,
                  augmentedMutationRecordEndLine = 42,
                  augmentedMutationRecordColStart = 10,
                  augmentedMutationRecordColEnd = 12,
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = [],
                  augmentedMutationRecordCoveringTests = Map.empty,
                  augmentedMutationRecordTimeoutMicros = 30000000
                }
            rendered = unlines (map (T.unpack . renderChunksText WithoutColours) (formatMutationLog (MutationId ["Foo.Bar", "ArithOp", "42", "10", "12", "extra"]) record))
        rendered
          `shouldBe` unlines
            [ "ArithOp at Foo/Bar.hs:42:10-12",
              "    - (+)",
              "    + (-)"
            ]

    describe "with 8 or more parts (alt-index suffix is only added on exactly 7)" $
      it "renders the header without an alt-index suffix when there are 8 parts" $ do
        let record =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"],
                  augmentedMutationRecordOperator = "ArithOp",
                  augmentedMutationRecordOriginal = "(+)",
                  augmentedMutationRecordReplacement = "(-)",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 42,
                  augmentedMutationRecordEndLine = 42,
                  augmentedMutationRecordColStart = 10,
                  augmentedMutationRecordColEnd = 12,
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = [],
                  augmentedMutationRecordCoveringTests = Map.empty,
                  augmentedMutationRecordTimeoutMicros = 30000000
                }
            rendered = unlines (map (T.unpack . renderChunksText WithoutColours) (formatMutationLog (MutationId ["Foo.Bar", "ArithOp", "42", "10", "12", "replStr", "2", "trailing"]) record))
        rendered
          `shouldBe` unlines
            [ "ArithOp at Foo/Bar.hs:42:10-12",
              "    - (+)",
              "    + (-)"
            ]

    describe "with record but no source context" $
      it "falls back to reconstructed path and expression-level diff" $ do
        let record =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"],
                  augmentedMutationRecordOperator = "ArithOp",
                  augmentedMutationRecordOriginal = "(+)",
                  augmentedMutationRecordReplacement = "(-)",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 42,
                  augmentedMutationRecordEndLine = 42,
                  augmentedMutationRecordColStart = 10,
                  augmentedMutationRecordColEnd = 12,
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = [],
                  augmentedMutationRecordCoveringTests = Map.empty,
                  augmentedMutationRecordTimeoutMicros = 30000000
                }
            rendered = unlines (map (T.unpack . renderChunksText WithoutColours) (formatMutationLog (MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"]) record))
        rendered
          `shouldBe` unlines
            [ "ArithOp at Foo/Bar.hs:42:10-12",
              "    - (+)",
              "    + (-)"
            ]

    -- Regression test for bug #4: when several alternatives share the same
    -- replacement text on the same span (e.g. ListLit's drop-first and
    -- drop-last on a 3-element list both rendering as "2 elements"), the
    -- 7-part MutationId carries a trailing alt-index.  The human-readable
    -- header must surface that index as " #<n>" so the two survivors do
    -- not render as identical lines in report.txt.
    describe "with 7-part MutationId (alt-index disambiguation)" $ do
      it "appends the alt-index suffix to the header line" $ do
        let record =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "ListLit", "19", "29", "38", "2 elements", "2"],
                  augmentedMutationRecordOperator = "ListLit",
                  augmentedMutationRecordOriginal = "3 elements",
                  augmentedMutationRecordReplacement = "2 elements",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 19,
                  augmentedMutationRecordEndLine = 19,
                  augmentedMutationRecordColStart = 29,
                  augmentedMutationRecordColEnd = 38,
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = [],
                  augmentedMutationRecordCoveringTests = Map.empty,
                  augmentedMutationRecordTimeoutMicros = 30000000
                }
            rendered = unlines (map (T.unpack . renderChunksText WithoutColours) (formatMutationLog (MutationId ["Foo.Bar", "ListLit", "19", "29", "38", "2 elements", "2"]) record))
        rendered
          `shouldBe` unlines
            [ "ListLit at Foo/Bar.hs:19:29-38 #2",
              "    - 3 elements",
              "    + 2 elements"
            ]

      it "renders sibling alternatives with distinct headers" $ do
        let record =
              AugmentedMutationRecord
                { augmentedMutationRecordId = MutationId ["Foo.Bar", "ListLit", "19", "29", "38", "2 elements", "1"],
                  augmentedMutationRecordOperator = "ListLit",
                  augmentedMutationRecordOriginal = "3 elements",
                  augmentedMutationRecordReplacement = "2 elements",
                  augmentedMutationRecordModule = "Foo.Bar",
                  augmentedMutationRecordLine = 19,
                  augmentedMutationRecordEndLine = 19,
                  augmentedMutationRecordColStart = 29,
                  augmentedMutationRecordColEnd = 38,
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = [],
                  augmentedMutationRecordCoveringTests = Map.empty,
                  augmentedMutationRecordTimeoutMicros = 30000000
                }
            firstId = MutationId ["Foo.Bar", "ListLit", "19", "29", "38", "2 elements", "1"]
            secondId = MutationId ["Foo.Bar", "ListLit", "19", "29", "38", "2 elements", "2"]
            renderFor mid =
              unlines (map (T.unpack . renderChunksText WithoutColours) (formatMutationLog mid record))
        renderFor firstId `shouldNotBe` renderFor secondId

    describe "with full source context" $
      it "produces git-diff style output with real source path" $
        goldenStringFile "test_resources/diff/mutation-log-with-context.txt" $
          pure $
            unlines $
              map (T.unpack . renderChunksText WithoutColours) $
                formatMutationLog
                  (MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"])
                  AugmentedMutationRecord
                    { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
                      augmentedMutationRecordOperator = "ArithOp",
                      augmentedMutationRecordOriginal = "+",
                      augmentedMutationRecordReplacement = "-",
                      augmentedMutationRecordModule = "Foo.Bar",
                      augmentedMutationRecordLine = 5,
                      augmentedMutationRecordEndLine = 5,
                      augmentedMutationRecordColStart = 14,
                      augmentedMutationRecordColEnd = 15,
                      augmentedMutationRecordSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
                      augmentedMutationRecordSourceLines = ["  result = x + y"],
                      augmentedMutationRecordMutatedLines = ["  result = x - y"],
                      augmentedMutationRecordContextBefore =
                        [ "add :: Int -> Int -> Int",
                          "add x y ="
                        ],
                      augmentedMutationRecordContextAfter =
                        [ "  in result"
                        ],
                      augmentedMutationRecordCoveringTests = Map.empty,
                      augmentedMutationRecordTimeoutMicros = 30000000
                    }

  describe "renderUnifiedDiff" $ do
    it "colours the differing characters within a paired line" $
      goldenStringFile "test_resources/diff/simple.txt" $
        pure $
          unlines $
            map (T.unpack . renderChunksText With8BitColours) $
              renderUnifiedDiff
                5
                ["bothPositive :: Int -> Int -> Bool"]
                ["bothPositive a b = a > 0 && b > 0"]
                ["bothPositive a b = a <= 0 && b > 0"]
                []

    it "colours intra-line diff with surrounding context" $
      goldenStringFile "test_resources/diff/with-context.txt" $
        pure $
          unlines $
            map (T.unpack . renderChunksText With8BitColours) $
              renderUnifiedDiff
                10
                ["add :: Int -> Int -> Int", "add x y ="]
                ["  let result = x + y in result"]
                ["  let result = x - y in result"]
                ["", "-- end"]

    it "renders unequal-size groups: deletes paired with adds plus extra deletes" $
      goldenStringFile "test_resources/diff/unequal-more-deletes.txt" $
        pure $
          unlines $
            map (T.unpack . renderChunksText With8BitColours) $
              renderUnifiedDiff
                1
                []
                ["alpha = 1", "beta = 2", "gamma = 3"]
                ["alpha = 11"]
                []

    it "renders unequal-size groups: extra adds after paired lines" $
      goldenStringFile "test_resources/diff/unequal-more-adds.txt" $
        pure $
          unlines $
            map (T.unpack . renderChunksText With8BitColours) $
              renderUnifiedDiff
                1
                []
                ["one"]
                ["one!", "two", "three"]
                []

    it "renders lines with no common characters using plain First/Second colours" $
      goldenStringFile "test_resources/diff/no-common.txt" $
        pure $
          unlines $
            map (T.unpack . renderChunksText With8BitColours) $
              renderUnifiedDiff
                1
                []
                ["xxxxx"]
                ["yyyyy"]
                []

    it "highlights only differing whitespace within otherwise-identical lines" $
      goldenStringFile "test_resources/diff/whitespace.txt" $
        pure $
          unlines $
            map (T.unpack . renderChunksText With8BitColours) $
              renderUnifiedDiff
                1
                []
                ["foo  bar"]
                ["foo bar"]
                []
