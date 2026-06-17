{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.SwitchFunctionArguments (theOperator) where

import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as T
import GHC
import GHC.Core.TyCo.Compare (eqType)
import GHC.Hs.Syn.Type (lhsExprType)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), SrcSpanDelta (..))

-- | Swap two arguments of the same type in a prefix function application.
--
-- When a function (or data constructor) is applied to two value arguments
-- that have the same type, the call still type-checks with those two
-- arguments swapped, but usually computes something different.  For every
-- pair of equal-typed arguments at an application site we emit one mutant
-- that swaps that pair.
--
-- Only prefix applications (@f a b@, @Con a b@) are considered: infix
-- operator applications are left to their own operators (e.g. 'Arith').  Two
-- restrictions keep the output honest:
--
--   * The operator only fires on the maximal application spine (it skips
--     sites that are themselves the function side of an enclosing
--     application, detected via 'instrumentEnvAppDepth').  Without this, the
--     curried sub-application @f a@ inside @f a b@ would re-emit the same
--     swap the full spine already covers.
--   * Pairs whose two arguments have identical source text are skipped:
--     swapping them produces an identical program and an unkillable mutant.
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "SwitchFunctionArguments",
      operatorDescription = "Swap two same-typed arguments of a function application",
      operatorMatch = \le -> case collectApp le of
        (headExpr, args)
          | pairs@(_ : _) <- sameTypePairs args ->
              Just (action le headExpr args pairs)
        _ -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  [LHsExpr GhcTc] ->
  [(Int, Int)] ->
  InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)]
action le headExpr args pairs = do
  -- Only fire on the outermost application spine.  Inside @f a b@, the
  -- function side @f a@ is visited with depth >= 1; emitting a swap there
  -- would duplicate one the full spine already produces.
  appDepth <- asks instrumentEnvAppDepth
  srcLines <- asks (maybe [] snd . instrumentEnvSourceFile)
  if appDepth /= 0
    then pure []
    else
      let ty = lhsExprType le
          haveSrc = not (null srcLines)
       in pure
            [ ( ty,
                foldl mkHsApp headExpr (swapAt i j argI argJ args),
                origStr,
                replStr,
                SwapSpans spanI spanJ
              )
            | (i, j) <- pairs,
              Just argI <- [indexArg args i],
              Just argJ <- [indexArg args j],
              RealSrcSpan spanI _ <- [getLocA argI],
              let textI = spanText srcLines spanI,
              RealSrcSpan spanJ _ <- [getLocA argJ],
              let textJ = spanText srcLines spanJ,
              -- Skip no-op swaps of textually identical arguments.
              not (haveSrc && textI == textJ),
              let origStr = label haveSrc i j textI textJ,
              let replStr = label haveSrc j i textJ textI
            ]
  where
    -- Human-readable manifest summary of the two operands, in the given
    -- order.  Falls back to positional names when the source isn't available.
    label haveSrc x y tx ty'
      | haveSrc = T.unpack tx ++ " " ++ T.unpack ty'
      | otherwise = "arg" ++ show (x + 1) ++ " " ++ "arg" ++ show (y + 1)

-- | The value arguments of a prefix application, in source order, together
-- with the function at the head.
--
-- The head is returned intact, retaining the type- and dictionary-application
-- wrappers the typechecker attached to it, so reapplying it to the swapped
-- arguments stays well-typed.
--
-- We deliberately do /not/ peel an enclosing 'HsPar': the parenthesis node
-- @(f a b)@ and the inner application @f a b@ are visited as separate
-- expressions by the walker (the walker recurses into an 'HsPar' with
-- 'instrumentLExpr', which re-runs every operator).  Peeling here would make
-- the operator fire on both, emitting the same swap twice.  Stopping at the
-- 'HsPar' means only the inner application produces the mutation.
collectApp :: LHsExpr GhcTc -> (LHsExpr GhcTc, [LHsExpr GhcTc])
collectApp = go []
  where
    go acc le = case unLoc le of
      HsApp _ f a -> go (a : acc) f
      _ -> (le, acc)

-- | Indices @(i, j)@ with @i < j@ whose arguments have the same type.
sameTypePairs :: [LHsExpr GhcTc] -> [(Int, Int)]
sameTypePairs args =
  let typed = zip [0 ..] (map lhsExprType args)
   in [ (i, j)
      | (i, ti) <- typed,
        (j, tj) <- typed,
        i < j,
        eqType ti tj
      ]

-- | Total list indexing without 'Prelude.!!'.
indexArg :: [a] -> Int -> Maybe a
indexArg xs i = case drop i xs of
  (x : _) -> Just x
  [] -> Nothing

-- | Replace the elements at indices @i@ and @j@ with @xj@ and @xi@.
swapAt :: Int -> Int -> a -> a -> [a] -> [a]
swapAt i j xi xj xs =
  [ if k == i then xj else if k == j then xi else x
  | (k, x) <- zip [0 ..] xs
  ]

-- | The exact source text covered by a 'RealSrcSpan'.  Multi-line spans are
-- joined with single spaces, which is enough for the no-op comparison and the
-- manifest summary.
spanText :: [Text] -> RealSrcSpan -> Text
spanText allLines rss =
  let startLine = srcSpanStartLine rss
      endLine = srcSpanEndLine rss
      colS = srcSpanStartCol rss
      colE = srcSpanEndCol rss
      lineN i = case drop (i - 1) allLines of
        (l : _) -> l
        [] -> T.empty
   in if startLine == endLine
        then T.take (colE - colS) (T.drop (colS - 1) (lineN startLine))
        else
          let firstPart = T.drop (colS - 1) (lineN startLine)
              middleParts = map lineN [startLine + 1 .. endLine - 1]
              lastPart = T.take (colE - 1) (lineN endLine)
           in T.intercalate " " (firstPart : middleParts ++ [lastPart])
