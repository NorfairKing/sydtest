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
        case le of
          L _ (HsVar _ (L _ v))
            | getOccString v `elem` ["True", "False"] -> Nothing
          _
            | tcEqType (lhsExprType le) boolTy -> Just action
            | otherwise -> Nothing
    }

action :: InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)]
action =
  pure
    [ (boolTy, nlHsDataCon trueDataCon, "e", "True", const (T.pack "True")),
      (boolTy, nlHsDataCon falseDataCon, "e", "False", const (T.pack "False"))
    ]
