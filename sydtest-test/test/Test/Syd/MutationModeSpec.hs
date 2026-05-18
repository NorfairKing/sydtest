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
import Text.Colour (Chunk, TerminalCapabilities (..), renderChunksText)

spec :: Spec
spec = do
  describe "formatMutationLog" $ do
    describe "fallback (wrong number of parts)" $
      it "shows the mutation id parts joined by slashes" $
        chunksToString (formatMutationLog (MutationId ["only-two", "parts"]) aRecord)
          `shouldBe` "only-two/parts\n"

    describe "with record but no source context" $
      it "falls back to reconstructed path and expression-level diff" $
        chunksToString
          ( formatMutationLog
              (MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"])
              aRecord
                { augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = []
                }
          )
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
      it "appends the alt-index suffix to the header line" $
        chunksToString
          ( formatMutationLog
              (MutationId ["Foo.Bar", "ListLit", "19", "29", "38", "2 elements", "2"])
              aRecord
                { augmentedMutationRecordOperator = "ListLit",
                  augmentedMutationRecordOriginal = "3 elements",
                  augmentedMutationRecordReplacement = "2 elements",
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = []
                }
          )
          `shouldBe` unlines
            [ "ListLit at Foo/Bar.hs:19:29-38 #2",
              "    - 3 elements",
              "    + 2 elements"
            ]

      it "renders sibling alternatives with distinct headers" $
        let mkRec replStr =
              aRecord
                { augmentedMutationRecordOperator = "ListLit",
                  augmentedMutationRecordOriginal = "3 elements",
                  augmentedMutationRecordReplacement = replStr,
                  augmentedMutationRecordSourceFile = Nothing,
                  augmentedMutationRecordSourceLines = [],
                  augmentedMutationRecordMutatedLines = [],
                  augmentedMutationRecordContextBefore = [],
                  augmentedMutationRecordContextAfter = []
                }
            firstId = MutationId ["Foo.Bar", "ListLit", "19", "29", "38", "2 elements", "1"]
            secondId = MutationId ["Foo.Bar", "ListLit", "19", "29", "38", "2 elements", "2"]
            first = chunksToString (formatMutationLog firstId (mkRec "2 elements"))
            second = chunksToString (formatMutationLog secondId (mkRec "2 elements"))
         in first `shouldNotBe` second

    describe "with full source context" $
      it "produces git-diff style output with real source path" $
        goldenStringFile "test_resources/diff/mutation-log-with-context.txt" $
          pure $
            chunksToString $
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
          colouredChunksToString $
            renderUnifiedDiff
              5
              ["bothPositive :: Int -> Int -> Bool"]
              ["bothPositive a b = a > 0 && b > 0"]
              ["bothPositive a b = a <= 0 && b > 0"]
              []

    it "colours intra-line diff with surrounding context" $
      goldenStringFile "test_resources/diff/with-context.txt" $
        pure $
          colouredChunksToString $
            renderUnifiedDiff
              10
              ["add :: Int -> Int -> Int", "add x y ="]
              ["  let result = x + y in result"]
              ["  let result = x - y in result"]
              ["", "-- end"]

    it "renders unequal-size groups: deletes paired with adds plus extra deletes" $
      goldenStringFile "test_resources/diff/unequal-more-deletes.txt" $
        pure $
          colouredChunksToString $
            renderUnifiedDiff
              1
              []
              ["alpha = 1", "beta = 2", "gamma = 3"]
              ["alpha = 11"]
              []

    it "renders unequal-size groups: extra adds after paired lines" $
      goldenStringFile "test_resources/diff/unequal-more-adds.txt" $
        pure $
          colouredChunksToString $
            renderUnifiedDiff
              1
              []
              ["one"]
              ["one!", "two", "three"]
              []

    it "renders lines with no common characters using plain First/Second colours" $
      goldenStringFile "test_resources/diff/no-common.txt" $
        pure $
          colouredChunksToString $
            renderUnifiedDiff
              1
              []
              ["xxxxx"]
              ["yyyyy"]
              []

    it "highlights only differing whitespace within otherwise-identical lines" $
      goldenStringFile "test_resources/diff/whitespace.txt" $
        pure $
          colouredChunksToString $
            renderUnifiedDiff
              1
              []
              ["foo  bar"]
              ["foo bar"]
              []

aRecord :: AugmentedMutationRecord
aRecord =
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

chunksToString :: [[Chunk]] -> String
chunksToString = unlines . map (T.unpack . renderChunksText WithoutColours)

colouredChunksToString :: [[Chunk]] -> String
colouredChunksToString = unlines . map (T.unpack . renderChunksText With8BitColours)
