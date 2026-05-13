{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.Negate (theOperator) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (boolTy)
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Tc.Utils.Env (tcLookupId)
import GHC.Tc.Utils.TcType (tcEqType)
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (lookupOccEnv, mkVarOcc)
import GHC.Types.Name.Reader (greName)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), liftTcM)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "Negate",
      operatorDescription = "Wrap a Bool-typed expression with not",
      operatorMatch = \le ->
        -- Don't match Bool literals or existing negations — BoolLit and the
        -- recursive instrumentation already handle those; wrapping them again
        -- would produce redundant mutations.
        case le of
          L _ (HsVar _ (L _ v))
            | getOccString v `elem` ["True", "False", "otherwise"] -> Nothing
          _
            | tcEqType (lhsExprType le) boolTy -> Just (action le)
            | otherwise -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)]
action le = do
  InstrumentEnv {instrRdrEnv} <- ask
  notId <- liftTcM $ case lookupOccEnv instrRdrEnv (mkVarOcc "not") of
    Just (gre : _) -> tcLookupId (greName gre)
    _ -> liftIO $ ioError $ userError "mutation/Negate: 'not' not in scope"
  let notVar = noLocA (HsVar NoExtField (noLocA notId))
      negated = mkHsApp notVar le
  pure [(boolTy, negated, "e", "not e", \origSpan -> T.pack "not (" <> origSpan <> T.pack ")")]
