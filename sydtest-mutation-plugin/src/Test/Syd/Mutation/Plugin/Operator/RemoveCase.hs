{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.RemoveCase (theOperator) where

import GHC
import GHC.Hs.Syn.Type (lhsExprType)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationAlt (..), MutationOperator (..), SrcSpanDelta (..))

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
  InstrM [MutationAlt]
action ann x scrut mgx lann alts ty =
  let n = length alts
      mkMutation i alt =
        let alts' = take i alts ++ drop (i + 1) alts
            mg' = MG mgx (L lann alts')
            removedSpan = case getLocA alt of
              RealSrcSpan rss _ -> [rss]
              UnhelpfulSpan _ -> []
         in MutationAlt
              { mutAltType = ty,
                mutAltExpr = L ann (HsCase x scrut mg'),
                mutAltOriginal = show n ++ " alternatives",
                mutAltReplacement = show (n - 1) ++ " alternatives (removed #" ++ show (i + 1) ++ ")",
                mutAltDelta = SpanRemoval removedSpan,
                mutAltMitigation = Nothing
              }
   in pure [mkMutation i alt | (i, alt) <- zip [0 ..] alts]
