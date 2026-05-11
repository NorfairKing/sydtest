{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.ListLit (theOperator) where

import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Builtin.Types (mkListTy)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "ListLit",
      operatorDescription = "Shrink a list literal by removing elements or emptying it",
      operatorMatch = \case
        (L ann (ExplicitList elTy es))
          | length es >= 2 ->
              Just (action ann elTy es)
        _ -> Nothing
    }

action ::
  SrcSpanAnnA ->
  Type ->
  [LHsExpr GhcTc] ->
  InstrM (NonEmpty (Type, LHsExpr GhcTc, String, String))
action ann elTy es =
  let listTy = mkListTy elTy
      n = length es
      mkList xs = (listTy, L ann (ExplicitList elTy xs), show n ++ " elements", show (length xs) ++ " elements")
      -- Always produce: empty list, drop-head.
      -- Only add drop-last if it gives a different length than drop-head
      -- (i.e. n > 2; when n == 2 both give one element).
      repls =
        mkList []
          : mkList (drop 1 es)
          : [mkList (take (n - 1) es) | n > 2]
   in pure $ case repls of
        (x : xs) -> x :| xs
        [] -> error "ListLit: no candidates"
