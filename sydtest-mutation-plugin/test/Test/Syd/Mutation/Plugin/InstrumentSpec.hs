{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.InstrumentSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import GHC.Data.FastString (mkFastString)
import GHC.Types.SrcLoc
import Test.Syd
import Test.Syd.Mutation.Plugin.Instrument

mkSpan :: Int -> Int -> Int -> Int -> RealSrcSpan
mkSpan startLine startCol endLine endCol =
  mkRealSrcSpan
    (mkRealSrcLoc (mkFastString "test.hs") startLine startCol)
    (mkRealSrcLoc (mkFastString "test.hs") endLine endCol)

spec :: Spec
spec = do
  describe "parseFunMutationAnns" $ do
    it "parses no annotations as no self-disable and no local disables" $
      parseFunMutationAnns []
        `shouldBe` FunMutationAnns (DisableOps []) Map.empty

    it "parses DisableMutations as self DisableAllOps" $
      parseFunMutationAnns ["DisableMutations"]
        `shouldBe` FunMutationAnns DisableAllOps Map.empty

    it "parses DisableMutations: A, B as self DisableOps [A,B]" $
      parseFunMutationAnns ["DisableMutations: BoolLit, ConstBool"]
        `shouldBe` FunMutationAnns (DisableOps ["BoolLit", "ConstBool"]) Map.empty

    it "parses DisableMutation: A as self DisableOps [A]" $
      parseFunMutationAnns ["DisableMutation: BoolLit"]
        `shouldBe` FunMutationAnns (DisableOps ["BoolLit"]) Map.empty

    it "parses DisableMutationsFor <name> as a local DisableAllOps entry" $
      parseFunMutationAnns ["DisableMutationsFor innerVar"]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" DisableAllOps)

    it "parses DisableMutationsFor <name>: A, B as a local DisableOps entry" $
      parseFunMutationAnns ["DisableMutationsFor innerVar: BoolLit, ConstBool"]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" (DisableOps ["BoolLit", "ConstBool"]))

    it "parses DisableMutationFor <name>: A as a local DisableOps [A] entry" $
      parseFunMutationAnns ["DisableMutationFor innerVar: BoolLit"]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" (DisableOps ["BoolLit"]))

    it "combines a self disable with a local disable" $
      parseFunMutationAnns
        [ "DisableMutations: BoolLit",
          "DisableMutationsFor innerVar: ConstBool"
        ]
        `shouldBe` FunMutationAnns
          (DisableOps ["BoolLit"])
          (Map.singleton "innerVar" (DisableOps ["ConstBool"]))

    it "merges two local disables for the same name into a combined DisableOps" $
      parseFunMutationAnns
        [ "DisableMutationFor innerVar: BoolLit",
          "DisableMutationFor innerVar: ConstBool"
        ]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" (DisableOps ["BoolLit", "ConstBool"]))

    it "merges a local DisableAllOps with a local DisableOps to DisableAllOps" $
      parseFunMutationAnns
        [ "DisableMutationsFor innerVar",
          "DisableMutationFor innerVar: BoolLit"
        ]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" DisableAllOps)

    it "keeps two distinct local-binding entries side by side" $
      parseFunMutationAnns
        [ "DisableMutationsFor a",
          "DisableMutationsFor b: BoolLit"
        ]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.fromList [("a", DisableAllOps), ("b", DisableOps ["BoolLit"])])

    it "ignores unrelated annotation strings" $
      parseFunMutationAnns
        [ "Not a mutation annotation",
          "DisableMutationsFor innerVar: BoolLit"
        ]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" (DisableOps ["BoolLit"]))

    it "ignores a DisableMutationsFor with an empty name" $
      parseFunMutationAnns ["DisableMutationsFor "]
        `shouldBe` FunMutationAnns (DisableOps []) Map.empty

  describe "applySpanRemoval" $ do
    it "removes the requested lines from a multi-line outer span" $
      applySpanRemoval
        ["one", "two", "three", "four"]
        1
        4
        [mkSpan 2 1 2 4]
        `shouldBe` ["one", "three", "four"]

    it "does not crash when the outer span extends past the end of allLines" $
      -- This reproduces the crash from a -pgmF preprocessor (e.g. sydtest-discover):
      -- the on-disk source file has only 1 line, but GHC's source spans refer to
      -- the generated source which is many lines longer.
      applySpanRemoval
        ["only one on-disk line"]
        1
        13
        [mkSpan 5 1 5 10]
        `shouldBe` ["only one on-disk line"]

    it "does not crash when outerStart is past the end of allLines" $
      applySpanRemoval
        ["one", "two"]
        10
        15
        [mkSpan 12 1 12 5]
        `shouldBe` []

    it "returns an empty list when there are no lines to keep" $
      applySpanRemoval
        ["one", "two", "three"]
        1
        3
        [mkSpan 1 1 3 5]
        `shouldBe` []

    it "handles multiple removed spans" $
      applySpanRemoval
        (map T.pack ["a", "b", "c", "d", "e"])
        1
        5
        [mkSpan 2 1 2 2, mkSpan 4 1 4 2]
        `shouldBe` ["a", "c", "e"]
