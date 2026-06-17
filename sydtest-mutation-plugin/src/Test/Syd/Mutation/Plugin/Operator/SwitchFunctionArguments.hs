{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.SwitchFunctionArguments (theOperator) where

import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC
import GHC.Core.TyCo.Compare (eqType)
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Types.Id (idName)
import GHC.Types.Name (getOccString, nameModule_maybe)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationAlt (..), MutationOperator (..), MutationOperatorKind (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.OptParse (OperatorConfig (..), operatorExtraStrings)

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
--
-- Symmetric functions (@max@, @min@, set union, a majority predicate, ...)
-- produce /equivalent/ mutants — swapping their arguments cannot change the
-- result, so no test can ever kill the mutant.  Because symmetry is a
-- semantic property the plugin cannot detect, calls to such functions are
-- suppressed by listing the function's name under the operator's
-- @skip-calls-to@ config key:
--
-- > operators:
-- >   SwitchFunctionArguments:
-- >     skip-calls-to:
-- >       - max
-- >       - Data.Set.union
--
-- A name matches either bare (@max@, matching any module) or fully qualified
-- (@GHC.Base.max@).
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "SwitchFunctionArguments",
      operatorDescription = "Swap two same-typed arguments of a function application",
      operatorKind = ExpressionOperator $ \le -> case collectApp le of
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
  InstrM [MutationAlt]
action le headExpr args pairs = do
  -- Only fire on the outermost application spine.  Inside @f a b@, the
  -- function side @f a@ is visited with depth >= 1; emitting a swap there
  -- would duplicate one the full spine already produces.
  appDepth <- asks instrumentEnvAppDepth
  srcLines <- asks (maybe [] snd . instrumentEnvSourceFile)
  -- Suppress calls to functions the user has marked symmetric (their swaps
  -- are equivalent, unkillable mutants).  See this module's haddock.
  opsConfig <- asks instrumentEnvOperatorsConfig
  let extra = maybe Map.empty operatorConfigExtra (Map.lookup "SwitchFunctionArguments" opsConfig)
      skipCallsTo = operatorExtraStrings "skip-calls-to" extra
      skipThisCall = case headFunctionName headExpr of
        Just n -> any (`elem` skipCallsTo) (nameMatchCandidates n)
        Nothing -> False
  if appDepth /= 0 || skipThisCall
    then pure []
    else
      let ty = lhsExprType le
          haveSrc = not (null srcLines)
          -- Hint shown for any surviving swap: if the called function is
          -- symmetric the mutant is equivalent and unkillable, and listing the
          -- function under 'skip-calls-to' suppresses it.
          mitigation = mitigationFor headExpr
       in pure
            [ MutationAlt
                { mutAltType = ty,
                  mutAltExpr = foldl mkHsApp headExpr (swapAt i j argI argJ args),
                  mutAltOriginal = origStr,
                  mutAltReplacement = replStr,
                  mutAltDelta = SwapSpans spanI spanJ,
                  mutAltMitigation = mitigation
                }
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

-- | The mitigation hint recorded on every swap mutation at a call: if the
-- called function turns out to be symmetric in these arguments the mutant is
-- equivalent (unkillable), and listing the function under @skip-calls-to@
-- suppresses it.  'Nothing' when the head is not a named function (so there is
-- no name to suggest), e.g. a data constructor.
mitigationFor :: LHsExpr GhcTc -> Maybe Text
mitigationFor headExpr = do
  n <- headFunctionName headExpr
  let fn = getOccString n
  pure $
    T.pack $
      concat
        [ "If `",
          fn,
          "` is symmetric in these arguments this is an equivalent mutant ",
          "that no test can kill; add `",
          fn,
          "` to this operator's `skip-calls-to` config to suppress it."
        ]

-- | The 'Name' of the function (or constructor) at the head of an
-- application, peeling the type- and dictionary-application wrappers the
-- typechecker leaves around the 'HsVar'.  'Nothing' for heads that are not a
-- plain variable (e.g. a lambda or a data constructor at 'ConLikeTc').
headFunctionName :: LHsExpr GhcTc -> Maybe Name
headFunctionName = go
  where
    go le = case unLoc le of
      HsVar _ (L _ v) -> Just (idName v)
      XExpr (WrapExpr (HsWrap _ e)) -> go (noLocA e)
      HsAppType _ f _ -> go f
      HsPar _ e -> go e
      _ -> Nothing

-- | The strings a @skip-calls-to@ entry may use to refer to a name: the bare
-- occurrence (@max@) and, when the name has a defining module, the fully
-- qualified form (@GHC.Base.max@).
nameMatchCandidates :: Name -> [Text]
nameMatchCandidates n =
  let occ = T.pack (getOccString n)
   in case nameModule_maybe n of
        Just m -> [occ, T.pack (moduleNameString (moduleName m)) <> "." <> occ]
        Nothing -> [occ]

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
