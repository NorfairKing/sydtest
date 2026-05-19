{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.ConstBool (theOperator) where

import Control.Monad.Reader (ask)
import GHC
import GHC.Builtin.Types (boolTy, falseDataCon, trueDataCon)
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Tc.Utils.TcType (tcEqType)
import GHC.Types.Name (getOccString)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), SrcSpanDelta (..))

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "ConstBool",
      operatorDescription = "Replace a Bool-typed expression with True or False",
      operatorMatch = \le ->
        if isBoolLit le
          then Nothing
          else
            if tcEqType (lhsExprType le) boolTy
              then Just action
              else Nothing
    }

-- | Returns True for boolean literals and 'otherwise' (which equals True),
-- unwrapping HsWrap and ConLikeTc nodes that GHC inserts after type-checking.
isBoolLit :: LHsExpr GhcTc -> Bool
isBoolLit = \case
  L _ (HsVar _ (L _ v)) -> getOccString v `elem` ["True", "False", "otherwise"]
  L _ (XExpr (ConLikeTc con _ _)) -> getOccString con `elem` ["True", "False"]
  L _ (XExpr (WrapExpr (HsWrap _ e))) -> isBoolLit (noLocA e)
  _ -> False

action :: InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)]
action = do
  InstrumentEnv {instrInGuard} <- ask
  let trueAlt = (boolTy, nlHsDataCon trueDataCon, "e", "True", TokenReplace "True")
      falseAlt = (boolTy, nlHsDataCon falseDataCon, "e", "False", TokenReplace "False")
  pure $ if instrInGuard then [trueAlt] else [trueAlt, falseAlt]
