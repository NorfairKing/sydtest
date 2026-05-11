{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.Arith (theOperator) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import GHC
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
            occ `elem` arithOps,
            Just ty <- lhsExprType l ->
              Just (action ty l op r occ)
        _ -> Nothing
    }

arithOps :: [String]
arithOps = ["+", "-", "*", "/"]

action ::
  Type ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  String ->
  InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)]
action ty l op r origOcc = do
  InstrumentEnv {instrRdrEnv} <- ask
  let replacements = filter (/= origOcc) arithOps
  mapM
    ( \replOcc -> do
        repl <- liftTcM $ mkOpReplacement instrRdrEnv l op r replOcc
        pure (ty, repl, origOcc, replOcc, const (T.pack replOcc))
    )
    replacements
