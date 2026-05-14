{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Syd.MutationModeSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Path
import Test.Syd
import Test.Syd.Mutation.AugmentedManifest (AugmentedMutationRecord (..))
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.MutationMode (formatMutationLog)
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

    describe "with full source context" $
      it "produces git-diff style output with real source path" $
        goldenStringFile "test_resources/mutation-log-with-context.txt" $
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
                    augmentedMutationRecordCoveringTests = Map.empty
                  }

aRecord :: AugmentedMutationRecord
aRecord =
  AugmentedMutationRecord
    { augmentedMutationRecordId = MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"],
      augmentedMutationRecordOperator = "ArithOp",
      augmentedMutationRecordOriginal = "(+)",
      augmentedMutationRecordReplacement = "(-)",
      augmentedMutationRecordModule = "Foo.Bar",
      augmentedMutationRecordLine = 42,
      augmentedMutationRecordColStart = 10,
      augmentedMutationRecordColEnd = 12,
      augmentedMutationRecordSourceFile = Nothing,
      augmentedMutationRecordSourceLines = [],
      augmentedMutationRecordMutatedLines = [],
      augmentedMutationRecordContextBefore = [],
      augmentedMutationRecordContextAfter = [],
      augmentedMutationRecordCoveringTests = Map.empty
    }

chunksToString :: [[Chunk]] -> String
chunksToString = unlines . map (T.unpack . renderChunksText WithoutColours)
