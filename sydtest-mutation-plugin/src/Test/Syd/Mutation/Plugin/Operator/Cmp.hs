{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.Cmp (theOperator) where

import Control.Monad.Reader (ask)
import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Builtin.Types (boolTy)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), liftTcM)
import Test.Syd.Mutation.Plugin.Operator.Util (mkOpReplacement, opOccName)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "Cmp",
      operatorDescription = "Replace any comparison operator with every other comparison operator",
      operatorMatch = \case
        (L _ (OpApp _ l op r))
          | Just occ <- opOccName op,
            occ `elem` cmpOps ->
              Just (action l op r occ)
        _ -> Nothing
    }

cmpOps :: [String]
cmpOps = ["==", "/=", "<", "<=", ">", ">="]

action ::
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  String ->
  InstrM (NonEmpty (Type, LHsExpr GhcTc, String, String))
action l op r origOcc = do
  InstrumentEnv {instrRdrEnv} <- ask
  let replacements = filter (/= origOcc) cmpOps
  repls <-
    mapM
      ( \replOcc -> do
          repl <- liftTcM $ mkOpReplacement instrRdrEnv l op r replOcc
          pure (boolTy, repl, origOcc, replOcc)
      )
      replacements
  case repls of
    (x : xs) -> pure (x :| xs)
    [] -> error "Cmp: empty replacement list"
