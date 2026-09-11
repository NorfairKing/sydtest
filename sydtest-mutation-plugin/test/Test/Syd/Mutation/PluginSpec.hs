{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.PluginSpec (spec) where

import qualified Data.Set as Set
import GHC.Data.FastString (mkFastString)
import GHC.Types.SrcLoc
import Test.Syd
import Test.Syd.Mutation.Plugin
import Test.Syd.Mutation.Plugin.Instrument

atLine :: Int -> SrcSpan
atLine line =
  let loc = mkSrcLoc (mkFastString "test.hs") line 1
   in mkSrcSpan loc loc

spec :: Spec
spec = do
  describe "parseModuleMutationAnns" $ do
    it "parses no annotations as no disable" $
      parseModuleMutationAnns []
        `shouldBe` ModuleMutationAnns (DisableOps []) []

    it "parses DisableMutations as DisableAllOps" $
      parseModuleMutationAnns ["DisableMutations"]
        `shouldBe` ModuleMutationAnns DisableAllOps []

    it "parses DisableMutations: A, B as the named operators" $
      parseModuleMutationAnns ["DisableMutations: BoolLit, ConstBool"]
        `shouldBe` ModuleMutationAnns (DisableOps ["BoolLit", "ConstBool"]) []

    it "merges the disables of several annotations" $
      parseModuleMutationAnns ["DisableMutation: BoolLit", "DisableMutation: ConstBool"]
        `shouldBe` ModuleMutationAnns (DisableOps ["BoolLit", "ConstBool"]) []

    it "ignores unrelated annotation strings" $
      parseModuleMutationAnns ["nocover"]
        `shouldBe` ModuleMutationAnns (DisableOps []) []

    it "keeps a malformed annotation" $
      parseModuleMutationAnns ["DisableMutationss: BoolLit"]
        `shouldBe` ModuleMutationAnns (DisableOps []) ["DisableMutationss: BoolLit"]

    it "keeps a DisableMutationsFor as malformed, since a module has no local bindings" $
      parseModuleMutationAnns ["DisableMutationsFor innerVar"]
        `shouldBe` ModuleMutationAnns (DisableOps []) ["DisableMutationsFor innerVar"]

  describe "deadModuleDisables" $ do
    it "reports nothing for a module with no mutation annotations" $
      deadModuleDisables
        (Set.fromList ["BoolLit"])
        (parseModuleMutationAnns [])
        Set.empty
        `shouldBe` []

    it "reports a module-wide disable of everything on a module nothing would mutate" $
      deadModuleDisables
        (Set.fromList ["BoolLit"])
        (parseModuleMutationAnns ["DisableMutations"])
        Set.empty
        `shouldBe` [DeadModuleDisable ScopeDeadAll]

    it "reports nothing for a module-wide disable of everything on a module something mutates" $
      deadModuleDisables
        (Set.fromList ["BoolLit"])
        (parseModuleMutationAnns ["DisableMutations"])
        (Set.fromList ["BoolLit"])
        `shouldBe` []

    it "reports a named operator that mutates nothing in the module" $
      deadModuleDisables
        (Set.fromList ["BoolLit", "ConstBool"])
        (parseModuleMutationAnns ["DisableMutation: BoolLit"])
        (Set.fromList ["ConstBool"])
        `shouldBe` [DeadModuleDisable (ScopeDeadOperator "BoolLit")]

    it "reports an operator name that is not an operator" $
      deadModuleDisables
        (Set.fromList ["BoolLit"])
        (parseModuleMutationAnns ["DisableMutation: BoolLt"])
        Set.empty
        `shouldBe` [DeadModuleDisable (ScopeUnknownOperator "BoolLt")]

    it "reports a malformed annotation" $
      deadModuleDisables
        (Set.fromList ["BoolLit"])
        (parseModuleMutationAnns ["DisableMutationss: BoolLit"])
        (Set.fromList ["BoolLit"])
        `shouldBe` [MalformedModuleAnnotation "DisableMutationss: BoolLit"]

  describe "renderDeadModuleDisable" $ do
    it "asks for a module-wide disable of everything to be removed" $
      renderDeadModuleDisable (Set.fromList ["BoolLit"]) (DeadModuleDisable ScopeDeadAll)
        `shouldBe` "Module-level mutation disable annotation disables every operator, but nothing in this module would be mutated. It disables no mutations; remove it."

    it "names the module-level forms when an annotation is none of them" $
      renderDeadModuleDisable
        (Set.fromList ["BoolLit"])
        (MalformedModuleAnnotation "DisableMutationsFor innerVar")
        `shouldBe` "Module-level mutation annotation `DisableMutationsFor innerVar` is none of the recognised module-level disable annotations, so it disables no mutations. Recognised forms are `DisableMutations`, `DisableMutation: <Operator>` and `DisableMutations: <Operator>, <Operator>`."

  describe "deadModuleDisableSpan" $ do
    it "points at the annotation whose operator is dead" $
      deadModuleDisableSpan
        (atLine 1)
        [("DisableMutation: ConstBool", atLine 5), ("DisableMutation: BoolLit", atLine 9)]
        (DeadModuleDisable (ScopeDeadOperator "BoolLit"))
        `shouldBe` atLine 9

    it "points at the annotation that disables everything" $
      deadModuleDisableSpan
        (atLine 1)
        [("DisableMutation: ConstBool", atLine 5), ("DisableMutations", atLine 9)]
        (DeadModuleDisable ScopeDeadAll)
        `shouldBe` atLine 9

    it "points at the malformed annotation itself" $
      deadModuleDisableSpan
        (atLine 1)
        [("DisableMutationss: BoolLit", atLine 5)]
        (MalformedModuleAnnotation "DisableMutationss: BoolLit")
        `shouldBe` atLine 5

    it "falls back when no recorded annotation accounts for the complaint" $
      -- A payload that is not a literal string is invisible in the parsed
      -- AST, so there is no span to point at.
      deadModuleDisableSpan
        (atLine 1)
        []
        (DeadModuleDisable (ScopeDeadOperator "BoolLit"))
        `shouldBe` atLine 1
