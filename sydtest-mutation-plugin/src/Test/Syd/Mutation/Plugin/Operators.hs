{-# LANGUAGE TemplateHaskell #-}

-- | Registry of all mutation operators.
--
-- To add a new operator:
--   1. Create @Test.Syd.Mutation.Plugin.Operator.<Name>@ exporting
--      @theOperator :: MutationOperator@.
--   2. Add one @import qualified@ line for it below.
--
-- The @allOperators@ list is assembled automatically from the operator
-- directory at compile time; only the import needs to be added by hand.
module Test.Syd.Mutation.Plugin.Operators (allOperators) where

import Test.Syd.Mutation.Plugin.Instrument (MutationOperator)
import qualified Test.Syd.Mutation.Plugin.Operator.Arith
import qualified Test.Syd.Mutation.Plugin.Operator.BoolLit
import qualified Test.Syd.Mutation.Plugin.Operator.Cmp
import qualified Test.Syd.Mutation.Plugin.Operator.ConstBool
import qualified Test.Syd.Mutation.Plugin.Operator.ConstEmptyList
import qualified Test.Syd.Mutation.Plugin.Operator.ConstNothing
import qualified Test.Syd.Mutation.Plugin.Operator.ElideCall
import qualified Test.Syd.Mutation.Plugin.Operator.IntLit
import qualified Test.Syd.Mutation.Plugin.Operator.ListLit
import qualified Test.Syd.Mutation.Plugin.Operator.LogicOp
import qualified Test.Syd.Mutation.Plugin.Operator.MaybeOp
import qualified Test.Syd.Mutation.Plugin.Operator.Negate
import qualified Test.Syd.Mutation.Plugin.Operator.RemoveAction
import qualified Test.Syd.Mutation.Plugin.Operator.RemoveCase
import qualified Test.Syd.Mutation.Plugin.Operator.RemoveClause
import qualified Test.Syd.Mutation.Plugin.Operator.SwitchFunctionArguments
import qualified Test.Syd.Mutation.Plugin.Operator.TupleSwap
import Test.Syd.Mutation.Plugin.Operators.TH (collectOperators)

allOperators :: [MutationOperator]
allOperators = $(collectOperators)
