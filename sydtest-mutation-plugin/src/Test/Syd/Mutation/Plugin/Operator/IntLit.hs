{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.IntLit (theOperator) where

import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Types.SourceText (il_value)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))
import Test.Syd.Mutation.Plugin.Operator.Util (mkIntLitExpr)

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
      repls = map (\r -> (ty, mkIntLitExpr r oltc, show n, show r)) candidates
   in pure $ case repls of
        (x : xs) -> x :| xs
        [] -> error "IntLit: no candidates (n is 0, 1, and -n simultaneously)"
