{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.CondFlip (theOperator) where

import qualified Data.Text as T
import GHC
import GHC.Hs.Syn.Type (lhsExprType)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "CondFlip",
      operatorDescription = "Swap the then and else branches of an if-then-else expression",
      operatorMatch = \case
        le@(L ann (HsIf x c t e)) ->
          Just (action ann x c t e (lhsExprType le))
        _ -> Nothing
    }

action ::
  SrcSpanAnnA ->
  XIf GhcTc ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  Type ->
  InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)]
action ann x c t e ty =
  let flipped = L ann (HsIf x c e t)
   in pure [(ty, flipped, "if c then t else e", "if c then e else t", id)]
