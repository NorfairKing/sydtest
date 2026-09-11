{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.InstrumentSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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
  describe "parseDisableAnn" $ do
    it "parses DisableMutations as a self DisableAllOps" $
      parseDisableAnn "DisableMutations" `shouldBe` AnnSelf DisableAllOps

    it "leaves a string that is not a mutation annotation alone" $
      parseDisableAnn "nocover" `shouldBe` AnnUnrelated

    it "reports a string that announces a mutation disable but does not parse as malformed" $
      parseDisableAnn "DisableMutationss: BoolLit" `shouldBe` AnnMalformed "DisableMutationss: BoolLit"

    it "reports a missing colon as malformed" $
      parseDisableAnn "DisableMutations BoolLit"
        `shouldBe` AnnMalformed "DisableMutations BoolLit"

    it "reports an empty operator list as malformed" $
      parseDisableAnn "DisableMutation:" `shouldBe` AnnMalformed "DisableMutation:"

    it "reports an empty entry in an operator list as malformed" $
      parseDisableAnn "DisableMutations: BoolLit, "
        `shouldBe` AnnMalformed "DisableMutations: BoolLit, "

    it "reports a DisableMutationsFor with an empty name as malformed" $
      parseDisableAnn "DisableMutationsFor " `shouldBe` AnnMalformed "DisableMutationsFor "

    it "ignores space around an operator name" $
      -- Space after the last operator is invisible in the pragma, so keeping
      -- it in the name would reject the annotation as naming no operator.
      parseDisableAnn "DisableMutations: BoolLit , ConstBool "
        `shouldBe` AnnSelf (DisableOps ["BoolLit", "ConstBool"])

    it "reports a singular DisableMutationFor without an operator as malformed" $
      -- The singular form promises exactly one operator, so stopping at the
      -- name names none.
      parseDisableAnn "DisableMutationFor innerVar"
        `shouldBe` AnnMalformed "DisableMutationFor innerVar"

  describe "parseFunMutationAnns" $ do
    it "parses no annotations as no self-disable and no local disables" $
      parseFunMutationAnns []
        `shouldBe` FunMutationAnns (DisableOps []) Map.empty []

    it "parses DisableMutations as self DisableAllOps" $
      parseFunMutationAnns ["DisableMutations"]
        `shouldBe` FunMutationAnns DisableAllOps Map.empty []

    it "parses DisableMutations: A, B as self DisableOps [A,B]" $
      parseFunMutationAnns ["DisableMutations: BoolLit, ConstBool"]
        `shouldBe` FunMutationAnns (DisableOps ["BoolLit", "ConstBool"]) Map.empty []

    it "parses DisableMutation: A as self DisableOps [A]" $
      parseFunMutationAnns ["DisableMutation: BoolLit"]
        `shouldBe` FunMutationAnns (DisableOps ["BoolLit"]) Map.empty []

    it "parses DisableMutationsFor <name> as a local DisableAllOps entry" $
      parseFunMutationAnns ["DisableMutationsFor innerVar"]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" DisableAllOps)
          []

    it "parses DisableMutationsFor <name>: A, B as a local DisableOps entry" $
      parseFunMutationAnns ["DisableMutationsFor innerVar: BoolLit, ConstBool"]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" (DisableOps ["BoolLit", "ConstBool"]))
          []

    it "parses DisableMutationFor <name>: A as a local DisableOps [A] entry" $
      parseFunMutationAnns ["DisableMutationFor innerVar: BoolLit"]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" (DisableOps ["BoolLit"]))
          []

    it "merges two self disables in the order they were annotated" $
      parseFunMutationAnns ["DisableMutation: BoolLit", "DisableMutation: ConstBool"]
        `shouldBe` FunMutationAnns (DisableOps ["BoolLit", "ConstBool"]) Map.empty []

    it "combines a self disable with a local disable" $
      parseFunMutationAnns
        [ "DisableMutations: BoolLit",
          "DisableMutationsFor innerVar: ConstBool"
        ]
        `shouldBe` FunMutationAnns
          (DisableOps ["BoolLit"])
          (Map.singleton "innerVar" (DisableOps ["ConstBool"]))
          []

    it "merges two local disables for the same name into a combined DisableOps" $
      parseFunMutationAnns
        [ "DisableMutationFor innerVar: BoolLit",
          "DisableMutationFor innerVar: ConstBool"
        ]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" (DisableOps ["BoolLit", "ConstBool"]))
          []

    it "merges a local DisableAllOps with a local DisableOps to DisableAllOps" $
      parseFunMutationAnns
        [ "DisableMutationsFor innerVar",
          "DisableMutationFor innerVar: BoolLit"
        ]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" DisableAllOps)
          []

    it "keeps two distinct local-binding entries side by side" $
      parseFunMutationAnns
        [ "DisableMutationsFor a",
          "DisableMutationsFor b: BoolLit"
        ]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.fromList [("a", DisableAllOps), ("b", DisableOps ["BoolLit"])])
          []

    it "ignores unrelated annotation strings" $
      parseFunMutationAnns
        [ "Not a mutation annotation",
          "DisableMutationsFor innerVar: BoolLit"
        ]
        `shouldBe` FunMutationAnns
          (DisableOps [])
          (Map.singleton "innerVar" (DisableOps ["BoolLit"]))
          []

    it "keeps a DisableMutationsFor with an empty name as malformed" $
      parseFunMutationAnns ["DisableMutationsFor "]
        `shouldBe` FunMutationAnns (DisableOps []) Map.empty ["DisableMutationsFor "]

  describe "deadInScope" $ do
    it "reports nothing for a scope that disables nothing" $
      deadInScope (Set.fromList ["BoolLit"]) (DisableOps []) Set.empty
        `shouldBe` []

    it "reports an operator name that is not an operator" $
      deadInScope (Set.fromList ["BoolLit"]) (DisableOps ["BoolLt"]) Set.empty
        `shouldBe` [ScopeUnknownOperator "BoolLt"]

    it "reports an unknown operator name as unknown rather than as dead" $
      -- The name is the problem; whether a non-operator fires is not a
      -- question worth answering.
      deadInScope (Set.fromList ["BoolLit"]) (DisableOps ["BoolLt"]) (Set.fromList ["BoolLit"])
        `shouldBe` [ScopeUnknownOperator "BoolLt"]

    it "reports a disable of the control mutation as the control mutation" $
      -- 'Control' is a name you see in a mutation report, so reaching for it
      -- in an annotation is a natural mistake, and answering "that is not a
      -- mutation operator" would contradict the report it came from.
      deadInScope (Set.fromList ["BoolLit"]) (DisableOps ["Control"]) (Set.fromList ["BoolLit"])
        `shouldBe` [ScopeControlOperator]

    it "reports a named operator that fires nowhere in the scope" $
      deadInScope
        (Set.fromList ["BoolLit", "ConstBool"])
        (DisableOps ["BoolLit"])
        (Set.fromList ["ConstBool"])
        `shouldBe` [ScopeDeadOperator "BoolLit"]

    it "reports nothing for a named operator that fires in the scope" $
      deadInScope
        (Set.fromList ["BoolLit", "ConstBool"])
        (DisableOps ["BoolLit"])
        (Set.fromList ["BoolLit"])
        `shouldBe` []

    it "reports each dead name once, in the order they were named" $
      deadInScope
        (Set.fromList ["BoolLit", "ConstBool", "Negate"])
        (DisableOps ["Negate", "BoolLit", "Negate", "ConstBool"])
        (Set.fromList ["ConstBool"])
        `shouldBe` [ScopeDeadOperator "Negate", ScopeDeadOperator "BoolLit"]

    it "reports an all-operator disable on a scope nothing would mutate" $
      deadInScope (Set.fromList ["BoolLit"]) DisableAllOps Set.empty
        `shouldBe` [ScopeDeadAll]

    it "reports nothing for an all-operator disable on a scope something mutates" $
      deadInScope (Set.fromList ["BoolLit"]) DisableAllOps (Set.fromList ["BoolLit"])
        `shouldBe` []

  describe "deadDisables" $ do
    it "reports nothing for a binding with no mutation annotations" $
      deadDisables
        (Set.fromList ["BoolLit"])
        (parseFunMutationAnns [])
        Set.empty
        Map.empty
        `shouldBe` []

    it "reports a declared target that matched no local binding" $
      deadDisables
        (Set.fromList ["BoolLit"])
        (parseFunMutationAnns ["DisableMutationsFor innerVar"])
        Set.empty
        Map.empty
        `shouldBe` [DeadTarget "innerVar"]

    it "reports nothing when the declared target was mutated by what it disables" $
      deadDisables
        (Set.fromList ["BoolLit"])
        (parseFunMutationAnns ["DisableMutationsFor innerVar"])
        Set.empty
        (Map.singleton "innerVar" (Set.fromList ["BoolLit"]))
        `shouldBe` []

    it "reports a target that exists but that the named operator does not mutate" $
      deadDisables
        (Set.fromList ["BoolLit", "ConstBool"])
        (parseFunMutationAnns ["DisableMutationFor innerVar: BoolLit"])
        Set.empty
        (Map.singleton "innerVar" (Set.fromList ["ConstBool"]))
        `shouldBe` [DeadLocal "innerVar" (ScopeDeadOperator "BoolLit")]

    it "reports a target that exists but would be mutated nowhere" $
      deadDisables
        (Set.fromList ["BoolLit"])
        (parseFunMutationAnns ["DisableMutationsFor innerVar"])
        Set.empty
        (Map.singleton "innerVar" Set.empty)
        `shouldBe` [DeadLocal "innerVar" ScopeDeadAll]

    it "reports only the targets that are dead" $
      deadDisables
        (Set.fromList ["BoolLit"])
        (parseFunMutationAnns ["DisableMutationsFor a", "DisableMutationsFor b"])
        Set.empty
        (Map.singleton "b" (Set.fromList ["BoolLit"]))
        `shouldBe` [DeadTarget "a"]

    it "reports a self-disable of an operator that mutates nothing in the binding" $
      deadDisables
        (Set.fromList ["BoolLit", "ConstBool"])
        (parseFunMutationAnns ["DisableMutation: BoolLit"])
        (Set.fromList ["ConstBool"])
        Map.empty
        `shouldBe` [DeadSelf (ScopeDeadOperator "BoolLit")]

    it "reports nothing for a self-disable of an operator that mutates the binding" $
      deadDisables
        (Set.fromList ["BoolLit"])
        (parseFunMutationAnns ["DisableMutation: BoolLit"])
        (Set.fromList ["BoolLit"])
        Map.empty
        `shouldBe` []

    it "reports a self-disable of everything on a binding that would be mutated nowhere" $
      deadDisables
        (Set.fromList ["BoolLit"])
        (parseFunMutationAnns ["DisableMutations"])
        Set.empty
        Map.empty
        `shouldBe` [DeadSelf ScopeDeadAll]

    it "reports a malformed annotation" $
      deadDisables
        (Set.fromList ["BoolLit"])
        (parseFunMutationAnns ["DisableMutationss: BoolLit"])
        Set.empty
        Map.empty
        `shouldBe` [MalformedAnnotation "DisableMutationss: BoolLit"]

    it "reports every complaint a binding's annotations earn at once" $
      deadDisables
        (Set.fromList ["BoolLit", "ConstBool"])
        ( parseFunMutationAnns
            [ "DisableMutation: BoolLit",
              "DisableMutationsFor gone",
              "DisableMutationss: ConstBool"
            ]
        )
        Set.empty
        Map.empty
        `shouldBe` [ MalformedAnnotation "DisableMutationss: ConstBool",
                     DeadSelf (ScopeDeadOperator "BoolLit"),
                     DeadTarget "gone"
                   ]

  describe "renderDeadDisable" $ do
    it "asks for a dead target to be removed" $
      renderDeadDisable (Set.fromList ["BoolLit"]) "myFun" (DeadTarget "innerVar")
        `shouldBe` "Mutation DisableMutationsFor annotation on `myFun` targets `innerVar`, which is not a local binding in its body. It disables no mutations; remove it."

    it "names the operators when an annotation names something else" $
      renderDeadDisable
        (Set.fromList ["ConstBool", "BoolLit"])
        "myFun"
        (DeadSelf (ScopeUnknownOperator "BoolLt"))
        `shouldBe` "Mutation disable annotation on `myFun` names `BoolLt`, which is not a mutation operator. The mutation operators are: BoolLit, ConstBool."

    it "names the local binding a dead local disable is aimed at" $
      renderDeadDisable
        (Set.fromList ["BoolLit"])
        "myFun"
        (DeadLocal "innerVar" ScopeDeadAll)
        `shouldBe` "Mutation disable annotation on `myFun` disables every operator, but nothing in `innerVar` would be mutated. It disables no mutations; remove it."

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

  describe "applySwapSpans" $ do
    it "swaps two single-line arguments of a prefix application" $
      -- "foo aaa bbb", swapping "aaa" (cols 5-8) and "bbb" (cols 9-12).
      applySwapSpans
        ["foo aaa bbb"]
        1
        1
        1
        12
        (mkSpan 1 5 1 8)
        (mkSpan 1 9 1 12)
        `shouldBe` ["foo bbb aaa"]

    it "swaps regardless of the order the spans are given in" $
      applySwapSpans
        ["foo aaa bbb"]
        1
        1
        1
        12
        (mkSpan 1 9 1 12)
        (mkSpan 1 5 1 8)
        `shouldBe` ["foo bbb aaa"]

    it "preserves text before and after the matched expression" $
      -- "  r = foo aaa bbb", the application spans cols 7-18.
      applySwapSpans
        ["  r = foo aaa bbb"]
        1
        1
        7
        18
        (mkSpan 1 11 1 14)
        (mkSpan 1 15 1 18)
        `shouldBe` ["  r = foo bbb aaa"]

    it "keeps a non-swapped middle argument in place" $
      -- "f aa bb cc", swapping the outer two ("aa" cols 3-5, "cc" cols 9-11).
      applySwapSpans
        ["f aa bb cc"]
        1
        1
        1
        11
        (mkSpan 1 3 1 5)
        (mkSpan 1 9 1 11)
        `shouldBe` ["f cc bb aa"]
