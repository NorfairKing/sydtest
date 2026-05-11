{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.MaybeOp (theOperator) where

import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Builtin.Types (nothingDataCon)
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Types.Name (getOccString)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "MaybeOp",
      operatorDescription = "Replace Just e with Nothing",
      operatorMatch = \case
        le@(L _ (HsApp _ f _))
          | Just occ <- funOccName f,
            occ == "Just" ->
              Just (action le)
        _ -> Nothing
    }

-- | Extract the OccName string of the function in an application,
-- handling type-checker wrappers.
funOccName :: LHsExpr GhcTc -> Maybe String
funOccName = \case
  L _ (HsVar _ (L _ v)) -> Just (getOccString v)
  L _ (XExpr (WrapExpr (HsWrap _ e))) -> funOccName (noLocA e)
  _ -> Nothing

action ::
  LHsExpr GhcTc ->
  InstrM (NonEmpty (Type, LHsExpr GhcTc, String, String))
action le =
  -- lhsExprType gives Maybe a, which is the type for the ifMutation wrapper.
  -- nlHsDataCon for nothingDataCon produces Nothing; the desugarer handles
  -- the polymorphic type instantiation via the surrounding type context.
  let mayTy = lhsExprType le
      nothingExpr = nlHsDataCon nothingDataCon
   in pure ((mayTy, nothingExpr, "Just e", "Nothing") :| [])
