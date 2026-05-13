{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.ConstBool (theOperator) where

import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (boolTy, falseDataCon, trueDataCon)
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Tc.Utils.TcType (tcEqType)
import GHC.Types.Name (getOccString)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))

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

-- | Returns True if the expression is a boolean literal (True or False),
-- unwrapping HsWrap nodes that GHC inserts after type-checking.
-- | Returns True for boolean literals and 'otherwise' (which equals True),
-- unwrapping HsWrap nodes that GHC inserts after type-checking.
isBoolLit :: LHsExpr GhcTc -> Bool
isBoolLit = \case
  L _ (HsVar _ (L _ v)) -> getOccString v `elem` ["True", "False", "otherwise"]
  L _ (XExpr (WrapExpr (HsWrap _ e))) -> isBoolLit (noLocA e)
  _ -> False

action :: InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)]
action =
  pure
    [ (boolTy, nlHsDataCon trueDataCon, "e", "True", const (T.pack "True")),
      (boolTy, nlHsDataCon falseDataCon, "e", "False", const (T.pack "False"))
    ]
