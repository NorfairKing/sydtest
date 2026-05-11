{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.IntLit (theOperator) where

import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Types.SourceText (il_value)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))
import Test.Syd.Mutation.Plugin.Operator.Util (mkIntLitReplacement)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "IntLit",
      operatorDescription = "Replace an integer literal n with 0, 1, and -n",
      operatorMatch = \case
        (L _ (HsOverLit _ (OverLit oltc@(OverLitTc {ol_type = ty}) (HsIntegral il)))) ->
          Just (action ty oltc (il_value il))
        _ -> Nothing
    }

action ::
  Type ->
  OverLitTc ->
  Integer ->
  InstrM (NonEmpty (Type, LHsExpr GhcTc, String, String))
action ty oltc n =
  let candidates = filter (/= n) [0, 1, negate n]
      alts = map (\r -> (ty, mkIntLitReplacement r oltc, show n, show r)) candidates
   in case alts of
        (x : xs) -> pure (x :| xs)
        [] -> pure ((ty, mkIntLitReplacement 0 oltc, show n, "0") :| [])
