module Test.Syd.Mutation.Plugin.Operator.RemoveGuard (theOperator) where

import GHC
import GHC.Builtin.Types (boolTy, trueDataCon)
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Tc.Utils.TcType (tcEqType)
import GHC.Types.Name (getOccString)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "RemoveGuard",
      operatorDescription = "Replace a Bool-typed expression with True",
      operatorMatch = \le ->
        case le of
          -- Don't replace True with True — that's a no-op.
          L _ (HsVar _ (L _ v))
            | getOccString v == "True" -> Nothing
          _
            | tcEqType (lhsExprType le) boolTy -> Just (action le)
            | otherwise -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  InstrM [(Type, LHsExpr GhcTc, String, String)]
action _ =
  pure [(boolTy, nlHsDataCon trueDataCon, "e", "True")]
