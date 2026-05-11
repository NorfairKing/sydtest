{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.MulToDiv (theOperator) where

import Control.Monad.Reader (ask)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import GHC
import GHC.Utils.Panic (panic)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), liftTcM)
import Test.Syd.Mutation.Plugin.Operator.Util (lhsExprType, mkOpReplacement, opOccName)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "MulToDiv",
      operatorDescription = "Replace multiplication (*) with division (/)",
      operatorMatch = \case
        (L _ (OpApp _ l op r))
          | opOccName op == Just "*" ->
              Just (action l op r)
        _ -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  InstrM (NonEmpty (Type, LHsExpr GhcTc, String, String))
action l op r = do
  InstrumentEnv {instrRdrEnv} <- ask
  repl <- liftTcM $ mkOpReplacement instrRdrEnv l op r "/"
  let ty = fromMaybe (panic "MulToDiv: no type on left operand") (lhsExprType l)
  pure ((ty, repl, "*", "/") :| [])
