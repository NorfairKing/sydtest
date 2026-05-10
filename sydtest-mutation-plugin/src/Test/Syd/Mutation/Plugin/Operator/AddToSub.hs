{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.AddToSub (theOperator) where

import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe)
import GHC
import GHC.Utils.Panic (panic)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), liftTcM)
import Test.Syd.Mutation.Plugin.Operator.Util (lhsExprType, mkOpReplacement, opOccName)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "AddToSub",
      operatorDescription = "Replace addition (+) with subtraction (-)",
      operatorMatch = \case
        (L _ (OpApp _ l op r))
          | opOccName op == Just "+" ->
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
  repl <- liftTcM $ mkOpReplacement instrRdrEnv l op r "-"
  let ty = fromMaybe (panic "AddToSub: no type on left operand") (lhsExprType l)
  pure (ty, repl, "+", "-")
