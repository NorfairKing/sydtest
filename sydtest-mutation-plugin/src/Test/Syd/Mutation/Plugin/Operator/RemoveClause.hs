{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.RemoveClause (theOperator) where

import GHC
import GHC.Builtin.Types (boolTy, falseDataCon, trueDataCon)
import Test.Syd.Mutation.Plugin.Instrument
  ( ClauseAlt (..),
    InstrM,
    MutationOperator (..),
    MutationOperatorKind (..),
    SrcSpanDelta (..),
    wrapWithIfMutation,
  )
import Test.Syd.Mutation.Runtime (MutationId)

-- | Remove one clause (equation) from a function binding with two or more
-- clauses.
--
-- A clause is a 'Match', not an 'LHsExpr', so this cannot be expressed through
-- the expression-level @ifMutation \@ty mutant original@ swap.  Instead, the
-- clause to be removed gets a guard @ifMutation mid False True@ prepended to
-- every one of its 'GRHS's: when @mid@ is the active mutation every guard
-- evaluates to 'False', so the clause matches no input and control falls
-- through to the next equation (or, for the last matching clause, to a
-- non-exhaustive @MatchFail@ that any covering test kills immediately).  When
-- @mid@ is inactive the guard is 'True' and 'ifMutation' also performs the
-- usual coverage bookkeeping, so no new runtime primitive is needed.
--
-- Because the injected guard sits after the clause's patterns, it is only
-- evaluated when those patterns match — exactly the inputs for which removing
-- the clause could change behaviour, so coverage is attributed to the tests
-- that actually reach the clause.
--
-- Only fires on bindings with at least two clauses: removing the sole clause of
-- a function is always a non-exhaustive crash, a trivially-killed mutant that
-- would only add noise.
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "RemoveClause",
      operatorDescription = "Remove one clause (equation) from a function binding",
      operatorKind = FunctionDeclarationOperator $ \case
        MG _ (L _ alts)
          | length alts >= 2 ->
              Just (pure (zipWith (mkAlt (length alts)) [0 ..] alts))
        _ -> Nothing
    }

-- | Build the removal alternative for clause @i@ (0-based) of @n@.
mkAlt :: Int -> Int -> LMatch GhcTc (LHsExpr GhcTc) -> ClauseAlt
mkAlt n i alt =
  ClauseAlt
    { clauseAltSpan = getLocA alt,
      clauseAltOriginal = show n ++ " clauses",
      clauseAltReplacement = show (n - 1) ++ " clauses (removed #" ++ show (i + 1) ++ ")",
      clauseAltDelta = SpanRemoval removedSpan,
      clauseAltMitigation = Nothing,
      clauseAltApply = injectRemovalGuard i
    }
  where
    removedSpan = case getLocA alt of
      RealSrcSpan rss _ -> [rss]
      UnhelpfulSpan _ -> []

-- | Prepend @ifMutation mid False True@ as a guard to every 'GRHS' of clause
-- @i@, so that whole clause is skipped when @mid@ is the active mutation.
injectRemovalGuard ::
  Int ->
  MutationId ->
  MatchGroup GhcTc (LHsExpr GhcTc) ->
  InstrM (MatchGroup GhcTc (LHsExpr GhcTc))
injectRemovalGuard i mid = \case
  MG x (L lann alts) -> do
    guardStmt <- mkRemovalGuardStmt mid
    let alts' = [if j == i then prependGuard guardStmt alt else alt | (j, alt) <- zip [0 ..] alts]
    pure (MG x (L lann alts'))

-- | Prepend a guard statement to every 'GRHS' of a clause.
prependGuard ::
  GuardLStmt GhcTc ->
  LMatch GhcTc (LHsExpr GhcTc) ->
  LMatch GhcTc (LHsExpr GhcTc)
prependGuard guardStmt = fmap $ \case
  Match mx ctx pats (GRHSs gx grhss localBinds) ->
    Match mx ctx pats (GRHSs gx (map (fmap addGuard) grhss) localBinds)
  where
    addGuard :: GRHS GhcTc (LHsExpr GhcTc) -> GRHS GhcTc (LHsExpr GhcTc)
    addGuard = \case
      GRHS gx guards body -> GRHS gx (guardStmt : guards) body

-- | The boolean guard statement @ifMutation mid False True@.
mkRemovalGuardStmt :: MutationId -> InstrM (GuardLStmt GhcTc)
mkRemovalGuardStmt mid = do
  guardExpr <- wrapWithIfMutation boolTy mid (nlHsDataCon falseDataCon) (nlHsDataCon trueDataCon)
  pure (noLocA (BodyStmt boolTy guardExpr noSyntaxExpr noSyntaxExpr))
