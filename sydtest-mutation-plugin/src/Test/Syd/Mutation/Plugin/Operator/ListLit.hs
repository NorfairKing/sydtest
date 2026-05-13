{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.ListLit (theOperator) where

import qualified Data.Text as T
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
  InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)]
action ann elTy es =
  let listTy = mkListTy elTy
      n = length es
      mkList xs replLabel =
        ( listTy,
          L ann (ExplicitList elTy xs),
          show n ++ " elements",
          replLabel,
          const (T.pack replLabel)
        )
      -- Always produce: empty list, drop-head.
      -- Only add drop-last if it gives a different length than drop-head
      -- (i.e. n > 2; when n == 2 both give one element).
      repls =
        mkList [] "[]"
          : mkList (drop 1 es) ("[" ++ show (n - 1) ++ " elements]")
          : [mkList (take (n - 1) es) ("[" ++ show (n - 1) ++ " elements]") | n > 2]
   in pure repls
