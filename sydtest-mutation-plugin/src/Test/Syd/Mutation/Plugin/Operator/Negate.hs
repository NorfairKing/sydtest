{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.Negate (theOperator) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import GHC
import GHC.Builtin.Types (boolTy)
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Tc.Utils.Env (tcLookupId)
import GHC.Tc.Utils.TcType (tcEqType)
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (lookupOccEnv, mkVarOcc)
import GHC.Types.Name.Reader (greName)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), SrcSpanDelta (..), liftTcM)

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
          L _ (XExpr (ConLikeTc con _ _))
            | getOccString con `elem` ["True", "False"] -> Nothing
          _
            | tcEqType (lhsExprType le) boolTy -> Just (action le)
            | otherwise -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)]
action le = do
  InstrumentEnv {instrumentEnvRdrEnv} <- ask
  notId <- liftTcM $ case lookupOccEnv instrumentEnvRdrEnv (mkVarOcc "not") of
    Just (gre : _) -> tcLookupId (greName gre)
    _ -> liftIO $ ioError $ userError "mutation/Negate: 'not' not in scope"
  let notVar = noLocA (HsVar NoExtField (noLocA notId))
      negated = mkHsApp notVar le
  -- Wrap with parentheses so the rendered @mutated_lines@ keeps the right
  -- precedence: @not n < 0@ reparses as @(not n) < 0@, but the AST mutant
  -- is @not (n < 0)@. The @WrapWithText@ delta produces @not (n < 0)@.
  pure [(boolTy, negated, "e", "not (e)", WrapWithText "not (" ")")]
