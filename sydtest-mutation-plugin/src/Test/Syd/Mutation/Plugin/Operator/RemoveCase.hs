{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.RemoveCase (theOperator) where

import qualified Data.Text as T
import GHC
import GHC.Hs.Syn.Type (lhsExprType)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "RemoveCase",
      operatorDescription = "Remove one alternative from a case expression",
      operatorMatch = \case
        le@(L ann (HsCase x scrut (MG mgx (L lann alts))))
          | length alts >= 2 ->
              Just (action ann x scrut mgx lann alts (lhsExprType le))
        _ -> Nothing
    }

action ::
  SrcSpanAnnA ->
  XCase GhcTc ->
  LHsExpr GhcTc ->
  XMG GhcTc (LHsExpr GhcTc) ->
  SrcSpanAnnL ->
  [LMatch GhcTc (LHsExpr GhcTc)] ->
  Type ->
  InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)]
action ann x scrut mgx lann alts ty =
  let n = length alts
      mkMutation i =
        let alts' = take i alts ++ drop (i + 1) alts
            mg' = MG mgx (L lann alts')
         in ( ty,
              L ann (HsCase x scrut mg'),
              show n ++ " alternatives",
              show (n - 1) ++ " alternatives (removed #" ++ show (i + 1) ++ ")",
              id
            )
   in pure [mkMutation i | i <- [0 .. n - 1]]
