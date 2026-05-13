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
          | length es >= 2,
            spanCoversFullList ann es ->
              Just (action ann elTy es)
        _ -> Nothing
    }

-- | Return True when @ann@'s span covers the full list content (not just the
-- opening @[@).  GHC's typechecked AST sometimes gives @ExplicitList@ a span
-- that only covers the @[@ token; in that case any source-level diff we
-- generate would be a no-op, so we skip those sites entirely.
spanCoversFullList :: SrcSpanAnnA -> [LHsExpr GhcTc] -> Bool
spanCoversFullList ann es =
  case (locA ann, map getLocA es) of
    (RealSrcSpan listRss _, elemSpans)
      | not (null elemSpans),
        Just lastElemRss <- lastRealSpan elemSpans ->
          srcSpanEndLine listRss > srcSpanStartLine lastElemRss
            || srcSpanEndCol listRss > srcSpanEndCol lastElemRss
    _ -> False
  where
    lastRealSpan [] = Nothing
    lastRealSpan ss = case last ss of
      RealSrcSpan rss _ -> Just rss
      UnhelpfulSpan _ -> Nothing

action ::
  SrcSpanAnnA ->
  Type ->
  [LHsExpr GhcTc] ->
  InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)]
action ann elTy es =
  let listTy = mkListTy elTy
      n = length es
      mkList xs = (listTy, L ann (ExplicitList elTy xs), show n ++ " elements", show (length xs) ++ " elements", id)
      -- Always produce: empty list, drop-head.
      -- Only add drop-last if it gives a different length than drop-head
      -- (i.e. n > 2; when n == 2 both give one element).
      repls =
        mkList []
          : mkList (drop 1 es)
          : [mkList (take (n - 1) es) | n > 2]
   in pure repls
