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
            | occ <- getOccString v,
              occ `elem` ["True", "False"] ->
                Just (action (Just occ))
          _
            | tcEqType (lhsExprType le) boolTy -> Just (action Nothing)
            | otherwise -> Nothing
    }

action ::
  -- | The literal value of the expression, if it is a Bool literal.
  Maybe String ->
  InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)]
action mLit =
  let alts =
        [ (boolTy, nlHsDataCon trueDataCon, "e", "True", const (T.pack "True")),
          (boolTy, nlHsDataCon falseDataCon, "e", "False", const (T.pack "False"))
        ]
   in pure $ filter (\(_, _, _, repl, _) -> Just repl /= mLit) alts
