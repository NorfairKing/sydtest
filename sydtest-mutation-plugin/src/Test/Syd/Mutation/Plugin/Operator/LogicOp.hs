{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.LogicOp (theOperator) where

import Control.Monad.Reader (ask)
import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Builtin.Types (boolTy)
import GHC.Utils.Panic (panic)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), liftTcM)
import Test.Syd.Mutation.Plugin.Operator.Util (mkOpReplacement, opOccName)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "LogicOp",
      operatorDescription = "Replace any boolean binary operator with every other boolean binary operator",
      operatorMatch = \case
        (L _ (OpApp _ l op r))
          | Just occ <- opOccName op,
            occ `elem` logicOps ->
              Just (action l op r occ)
        _ -> Nothing
    }

logicOps :: [String]
logicOps = ["&&", "||"]

action ::
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  String ->
  InstrM (NonEmpty (Type, LHsExpr GhcTc, String, String))
action l op r origOcc = do
  InstrumentEnv {instrRdrEnv} <- ask
  let replacements = filter (/= origOcc) logicOps
  repls <-
    mapM
      ( \replOcc -> do
          repl <- liftTcM $ mkOpReplacement instrRdrEnv l op r replOcc
          pure (boolTy, repl, origOcc, replOcc)
      )
      replacements
  pure $ case repls of
    (x : xs) -> x :| xs
    [] -> panic "LogicOp: no replacements"
