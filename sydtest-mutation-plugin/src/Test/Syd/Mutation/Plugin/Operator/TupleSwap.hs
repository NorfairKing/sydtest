{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.TupleSwap (theOperator) where

import Control.Monad.Reader (asks)
import qualified Data.Text as T
import GHC
import GHC.Core.TyCo.Compare (eqType)
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Types.Basic (Boxity (..))
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationAlt (..), MutationOperator (..), MutationOperatorKind (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (spanText)

-- | Swap two same-typed components of a tuple.
--
-- For a tuple @(e1, ..., eN)@ whose components @ei@ and @ej@ have the same
-- type, the tuple still type-checks with those two components swapped but
-- usually denotes a different value, so we emit one mutant per same-typed pair
-- that swaps that pair.
--
-- This is the tuple-syntax counterpart to 'SwitchFunctionArguments': an
-- 'ExplicitTuple' is its own AST node, not an application spine, so that
-- operator never reaches it.  Two restrictions keep the output honest,
-- mirroring that operator:
--
--   * Only fully-saturated tuples are mutated.  A tuple /section/ like @(,b)@
--     carries a 'Missing' component and is really a function, so it is skipped.
--   * Pairs whose two components have identical source text are skipped:
--     swapping them produces an identical program and an unkillable mutant.
--
-- Only /boxed/ tuples are mutated.  An unboxed tuple's type has kind
-- @TYPE (TupleRep ...)@, not @Type@, but the @ifMutation \@ty@ wrapper the
-- plugin places around every mutant is monomorphic in a lifted @ty@.  Wrapping
-- an unboxed tuple builds an ill-kinded application that GHC miscompiles into a
-- binary that aborts at runtime, so unboxed tuples are excluded outright.
--
-- Unlike 'SwitchFunctionArguments' no application-depth guard is needed: each
-- 'ExplicitTuple' node is visited exactly once by the walker, and a nested
-- tuple is a distinct node with a distinct span, so no swap is emitted twice.
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "TupleSwap",
      operatorDescription = "Swap two same-typed components of a tuple",
      operatorKind = ExpressionOperator $ \case
        le@(L _ (ExplicitTuple _ args Boxed))
          | Just exprs <- allPresent args,
            length exprs >= 2,
            pairs@(_ : _) <- sameTypePairs exprs ->
              Just (action le exprs pairs)
        _ -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  [LHsExpr GhcTc] ->
  [(Int, Int)] ->
  InstrM [MutationAlt]
action le exprs pairs = do
  srcLines <- asks (maybe [] snd . instrumentEnvSourceFile)
  let ty = lhsExprType le
      haveSrc = not (null srcLines)
  pure
    [ MutationAlt
        { mutAltType = ty,
          mutAltExpr = rebuild i j,
          mutAltOriginal = origStr,
          mutAltReplacement = replStr,
          mutAltDelta = SwapSpans spanI spanJ,
          mutAltMitigation = Nothing
        }
    | (i, j) <- pairs,
      Just exprI <- [indexList exprs i],
      Just exprJ <- [indexList exprs j],
      RealSrcSpan spanI _ <- [getLocA exprI],
      let textI = spanText srcLines spanI,
      RealSrcSpan spanJ _ <- [getLocA exprJ],
      let textJ = spanText srcLines spanJ,
      -- Skip no-op swaps of textually identical components.
      not (haveSrc && textI == textJ),
      let origStr = label haveSrc i j textI textJ,
      let replStr = label haveSrc j i textJ textI
    ]
  where
    -- Rebuild the tuple with the components at indices @i@ and @j@ swapped,
    -- reusing the original node's extension field and boxity.
    rebuild i j = case le of
      L ann (ExplicitTuple x args boxity) ->
        L ann (ExplicitTuple x (swapAt i j args) boxity)
      _ -> le
    -- Human-readable manifest summary of the two components, in the given
    -- order.  Falls back to positional names when the source isn't available.
    label haveSrc p q tp tq
      | haveSrc = T.unpack tp ++ ", " ++ T.unpack tq
      | otherwise = "arg" ++ show (p + 1) ++ ", arg" ++ show (q + 1)

-- | The component expressions of a tuple, if every component is present (i.e.
-- it is a real tuple, not a tuple section).
allPresent :: [HsTupArg GhcTc] -> Maybe [LHsExpr GhcTc]
allPresent = traverse $ \case
  Present _ e -> Just e
  Missing _ -> Nothing

-- | Indices @(i, j)@ with @i < j@ whose components have the same type.
sameTypePairs :: [LHsExpr GhcTc] -> [(Int, Int)]
sameTypePairs exprs =
  let typed = zip [0 ..] (map lhsExprType exprs)
   in [ (i, j)
      | (i, ti) <- typed,
        (j, tj) <- typed,
        i < j,
        eqType ti tj
      ]

-- | Total list indexing without 'Prelude.!!'.
indexList :: [a] -> Int -> Maybe a
indexList xs i = case drop i xs of
  (x : _) -> Just x
  [] -> Nothing

-- | Swap the elements at indices @i@ and @j@.
swapAt :: Int -> Int -> [a] -> [a]
swapAt i j xs = case (indexList xs i, indexList xs j) of
  (Just xi, Just xj) ->
    [ if k == i then xj else if k == j then xi else x
    | (k, x) <- zip [0 ..] xs
    ]
  _ -> xs
