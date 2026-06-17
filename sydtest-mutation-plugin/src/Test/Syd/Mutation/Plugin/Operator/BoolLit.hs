{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.BoolLit (theOperator) where

import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (boolTy, falseDataCon, trueDataCon)
import GHC.Types.Name (getOccString)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationAlt (..), MutationOperator (..), MutationOperatorKind (..), SrcSpanDelta (..))

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "BoolLit",
      operatorDescription = "Replace a boolean literal with its negation",
      operatorKind = ExpressionOperator $ fmap action . extractBoolLit
    }

-- | Extract the OccName of a boolean literal, unwrapping HsWrap and
-- ConLikeTc nodes that GHC inserts after type-checking.
extractBoolLit :: LHsExpr GhcTc -> Maybe String
extractBoolLit = \case
  L _ (HsVar _ (L _ v))
    | occ <- getOccString v, occ `elem` boolLits -> Just occ
  L _ (XExpr (ConLikeTc con _ _))
    | occ <- getOccString con, occ `elem` boolLits -> Just occ
  L _ (XExpr (WrapExpr (HsWrap _ e))) -> extractBoolLit (noLocA e)
  _ -> Nothing

boolLits :: [String]
boolLits = ["True", "False"]

action ::
  String ->
  InstrM [MutationAlt]
action origOcc =
  let repl = if origOcc == "True" then falseDataCon else trueDataCon
      replOcc = if origOcc == "True" then "False" else "True"
      replExpr = nlHsDataCon repl
   in pure
        [ MutationAlt
            { mutAltType = boolTy,
              mutAltExpr = replExpr,
              mutAltOriginal = origOcc,
              mutAltReplacement = replOcc,
              mutAltDelta = TokenReplace (T.pack replOcc),
              mutAltMitigation = Nothing
            }
        ]
