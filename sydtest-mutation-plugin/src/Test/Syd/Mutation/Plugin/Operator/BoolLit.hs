{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.BoolLit (theOperator) where

import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Builtin.Types (boolTy, falseDataCon, trueDataCon)
import GHC.Types.Name (getOccString)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "BoolLit",
      operatorDescription = "Replace a boolean literal with its negation",
      operatorMatch = \case
        (L _ (HsVar _ (L _ v)))
          | occ <- getOccString v,
            occ `elem` boolLits ->
              Just (action occ)
        _ -> Nothing
    }

boolLits :: [String]
boolLits = ["True", "False"]

action ::
  String ->
  InstrM (NonEmpty (Type, LHsExpr GhcTc, String, String))
action origOcc =
  let repl = if origOcc == "True" then falseDataCon else trueDataCon
      replOcc = if origOcc == "True" then "False" else "True"
      replExpr = nlHsDataCon repl
   in pure ((boolTy, replExpr, origOcc, replOcc) :| [])
