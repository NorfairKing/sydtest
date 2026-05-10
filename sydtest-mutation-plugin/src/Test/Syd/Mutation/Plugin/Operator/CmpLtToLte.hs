{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.CmpLtToLte (theOperator) where

import Control.Monad.Reader (ask)
import GHC
import GHC.Builtin.Types (boolTy)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), liftTcM)
import Test.Syd.Mutation.Plugin.Operator.Util (mkOpReplacement, opOccName)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "CmpLtToLte",
      operatorDescription = "Replace strict less-than (<) with less-than-or-equal (<=)",
      operatorMatch = \case
        (L _ (OpApp _ l op r))
          | opOccName op == Just "<" ->
              Just (action l op r)
        _ -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  InstrM (Type, LHsExpr GhcTc, String, String)
action l op r = do
  InstrumentEnv {instrRdrEnv} <- ask
  repl <- liftTcM $ mkOpReplacement instrRdrEnv l op r "<="
  pure (boolTy, repl, "<", "<=")
