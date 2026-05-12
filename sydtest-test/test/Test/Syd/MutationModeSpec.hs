{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Syd.MutationModeSpec (spec) where

import Path
import Test.Syd
import Test.Syd.Mutation.Manifest (MutationRecord (..))
import Test.Syd.Mutation.Runtime (MutationId (..))
import Test.Syd.MutationMode (formatMutationLog)

spec :: Spec
spec = do
  describe "formatMutationLog" $ do
    describe "fallback (no record)" $
      it "shows the mutation id parts joined by slashes" $
        formatMutationLog (MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"]) Nothing
          `shouldBe` "Testing mutation Foo.Bar/ArithOp/42/10/12"

    describe "fallback (wrong number of parts)" $
      it "shows the mutation id parts joined by slashes" $
        formatMutationLog (MutationId ["only-two", "parts"]) (Just aRecord)
          `shouldBe` "Testing mutation only-two/parts"

    describe "with record but no source context" $
      it "falls back to reconstructed path and expression-level diff" $
        formatMutationLog
          (MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"])
          ( Just
              aRecord
                { mutRecSourceFile = Nothing,
                  mutRecSourceLine = Nothing,
                  mutRecMutatedLine = Nothing,
                  mutRecContextBefore = [],
                  mutRecContextAfter = []
                }
          )
          `shouldBe` unlines
            [ "Testing mutation ArithOp at Foo/Bar.hs:42:10-12:",
              "    - (+)",
              "    + (-)"
            ]

    describe "with full source context" $
      it "produces git-diff style output with real source path" $
        goldenStringFile "test_resources/mutation-log-with-context.txt" $
          pure $
            formatMutationLog
              (MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"])
              ( Just
                  MutationRecord
                    { mutRecId = MutationId ["Foo.Bar", "ArithOp", "5", "14", "15"],
                      mutRecOperator = "ArithOp",
                      mutRecOriginal = "+",
                      mutRecReplacement = "-",
                      mutRecModule = "Foo.Bar",
                      mutRecLine = 5,
                      mutRecColStart = 14,
                      mutRecColEnd = 15,
                      mutRecSourceFile = Just $(mkRelFile "src/Foo/Bar.hs"),
                      mutRecSourceLine = Just "  result = x + y",
                      mutRecMutatedLine = Just "  result = x - y",
                      mutRecContextBefore =
                        [ "add :: Int -> Int -> Int",
                          "add x y ="
                        ],
                      mutRecContextAfter =
                        [ "  in result"
                        ],
                      mutRecCoveringTests = Nothing
                    }
              )

aRecord :: MutationRecord
aRecord =
  MutationRecord
    { mutRecId = MutationId ["Foo.Bar", "ArithOp", "42", "10", "12"],
      mutRecOperator = "ArithOp",
      mutRecOriginal = "(+)",
      mutRecReplacement = "(-)",
      mutRecModule = "Foo.Bar",
      mutRecLine = 42,
      mutRecColStart = 10,
      mutRecColEnd = 12,
      mutRecSourceFile = Nothing,
      mutRecSourceLine = Nothing,
      mutRecMutatedLine = Nothing,
      mutRecContextBefore = [],
      mutRecContextAfter = [],
      mutRecCoveringTests = Nothing
    }
