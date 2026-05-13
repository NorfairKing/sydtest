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
            spanIsSingleLine ann ->
              Just (action ann elTy es)
        _ -> Nothing
    }

-- | Return True when @ann@'s span fits on a single source line.
-- Multi-line list spans cannot be displayed correctly by the single-line diff
-- logic in 'recordMutation', so we skip those sites entirely to avoid
-- generating no-op diffs.
spanIsSingleLine :: SrcSpanAnnA -> Bool
spanIsSingleLine ann =
  case locA ann of
    RealSrcSpan rss _ -> srcSpanStartLine rss == srcSpanEndLine rss
    UnhelpfulSpan _ -> False

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
