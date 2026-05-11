{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Test.Syd.Mutation.Plugin.Operator.IntLit (theOperator) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes)
import GHC
import GHC.Types.SourceText (il_value)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..), liftTcM)
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
  InstrM (Maybe (NonEmpty (Type, LHsExpr GhcTc, String, String)))
action ty oltc n = do
  let candidates = filter (/= n) [0, 1, negate n]
  mrepls <- mapM (\r -> fmap (fmap (ty,,show n,show r)) (liftTcM (mkIntLitReplacement r oltc))) candidates
  pure $ case catMaybes mrepls of
    (x : xs) -> Just (x :| xs)
    [] -> Nothing
