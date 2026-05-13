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
      mkList xs transform =
        ( listTy,
          L ann (ExplicitList elTy xs),
          show n ++ " elements",
          show (length xs) ++ " elements",
          transform
        )
      -- Always produce: empty list, drop-head.
      -- Only add drop-last if it gives a different length than drop-head
      -- (i.e. n > 2; when n == 2 both give one element).
      repls =
        mkList [] (const (T.pack "[]"))
          : mkList (drop 1 es) dropHead
          : [mkList (take (n - 1) es) dropLast | n > 2]
   in pure repls

-- | Given the full source text of a list literal, remove the first element.
-- e.g. "[ a,\n  b,\n  c\n]" -> "[ b,\n  c\n]"
dropHead :: T.Text -> T.Text
dropHead src =
  case splitAtFirstTopLevelComma src of
    Nothing -> src
    Just (_, rest) -> T.pack "[" <> T.stripStart rest

-- | Given the full source text of a list literal, remove the last element.
-- e.g. "[ a,\n  b,\n  c\n]" -> "[ a,\n  b\n]"
dropLast :: T.Text -> T.Text
dropLast src =
  case splitAtLastTopLevelComma src of
    Nothing -> src
    Just (before, _) -> T.stripEnd before <> T.pack "\n]"

-- | Split at the first top-level comma (depth 0 w.r.t. brackets/parens/quotes).
splitAtFirstTopLevelComma :: T.Text -> Maybe (T.Text, T.Text)
splitAtFirstTopLevelComma = go (0 :: Int) T.empty . T.unpack
  where
    go _ _ [] = Nothing
    go depth acc (c : cs)
      | c == ',' && depth == 0 = Just (T.reverse acc, T.pack cs)
      | c `elem` ("([{" :: String) = go (depth + 1) (T.cons c acc) cs
      | c `elem` (")]}" :: String) = go (max 0 (depth - 1)) (T.cons c acc) cs
      | otherwise = go depth (T.cons c acc) cs

-- | Split at the last top-level comma.
splitAtLastTopLevelComma :: T.Text -> Maybe (T.Text, T.Text)
splitAtLastTopLevelComma src =
  case splitAtFirstTopLevelComma (T.reverse src) of
    Nothing -> Nothing
    Just (after, before) -> Just (T.reverse before, T.reverse after)
