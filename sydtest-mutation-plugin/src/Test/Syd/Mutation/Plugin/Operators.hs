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
import qualified Test.Syd.Mutation.Plugin.Operator.AddToSub
import qualified Test.Syd.Mutation.Plugin.Operator.Cmp
import qualified Test.Syd.Mutation.Plugin.Operator.DivToMul
import qualified Test.Syd.Mutation.Plugin.Operator.IntLit1To0
import qualified Test.Syd.Mutation.Plugin.Operator.IntLitNTo0
import qualified Test.Syd.Mutation.Plugin.Operator.IntLitNTo1
import qualified Test.Syd.Mutation.Plugin.Operator.IntLitNegate
import qualified Test.Syd.Mutation.Plugin.Operator.MulToDiv
import qualified Test.Syd.Mutation.Plugin.Operator.SubToAdd
import Test.Syd.Mutation.Plugin.Operators.TH (collectOperators)

allOperators :: [MutationOperator]
allOperators = $(collectOperators)
