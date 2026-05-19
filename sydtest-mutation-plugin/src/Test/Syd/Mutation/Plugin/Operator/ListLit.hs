{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.ListLit (theOperator) where

import GHC
import GHC.Builtin.Types (mkListTy)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..), SrcSpanDelta (..))

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
  InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)]
action ann elTy es =
  let listTy = mkListTy elTy
      n = length es
      toRss e = case getLocA e of
        RealSrcSpan rss _ -> [rss]
        UnhelpfulSpan _ -> []
      mkList xs delta =
        ( listTy,
          L ann (ExplicitList elTy xs),
          show n ++ " elements",
          show (length xs) ++ " elements",
          delta
        )
      -- Always produce: empty list, drop-head.
      -- Only add drop-last if it gives a different length than drop-head
      -- (i.e. n > 2; when n == 2 both give one element).
      lastE = reverse es
      repls = case es of
        [] -> []
        (firstE : restEs) ->
          mkList [] (SpanRemoval (concatMap toRss es))
            : mkList restEs (SpanRemoval (toRss firstE))
            : case lastE of
              [] -> []
              (le : _) -> [mkList (take (n - 1) es) (SpanRemoval (toRss le)) | n > 2]
   in pure repls
