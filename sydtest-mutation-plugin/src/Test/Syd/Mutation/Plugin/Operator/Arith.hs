{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.Arith (theOperator) where

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
    { operatorName = "Arith",
      operatorDescription = "Replace any binary arithmetic operator with every other arithmetic operator",
      operatorMatch = \case
        (L _ (OpApp _ l op r))
          | Just occ <- opOccName op,
            occ `elem` arithOps ->
              Just (action l op r occ)
        _ -> Nothing
    }

arithOps :: [String]
arithOps = ["+", "-", "*", "/"]

action ::
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  String ->
  InstrM (NonEmpty (Type, LHsExpr GhcTc, String, String))
action l op r origOcc = do
  InstrumentEnv {instrRdrEnv} <- ask
  let ty = fromMaybe (panic "Arith: no type on left operand") (lhsExprType l)
      replacements = filter (/= origOcc) arithOps
  repls <-
    mapM
      ( \replOcc -> do
          repl <- liftTcM $ mkOpReplacement instrRdrEnv l op r replOcc
          pure (ty, repl, origOcc, replOcc)
      )
      replacements
  case repls of
    (x : xs) -> pure (x :| xs)
    [] -> error "Arith: empty replacement list"
