{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.RemoveAction (theOperator) where

import Control.Monad.Reader (asks)
import GHC
import GHC.Hs.Syn.Type (lhsExprType)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationAlt (..), MutationOperator (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (opOccName)

-- | Remove one non-binding action from a do block.
--
-- At GhcTc, GHC 9.10 has already desugared @do@ blocks into chains of
-- @(>>)@ applications, so we never see an @HsDo@ node here for the typical
-- @do { tell "Hello"; tell "!" }@ shape. Instead we recognise the
-- desugared form @a >> rest@ (an 'HsApp' chain whose head is @(>>)@) and
-- produce a mutation that drops the left-hand action, yielding just
-- @rest@.
--
-- Type-safety note: @(>>) :: m a -> m b -> m b@, so dropping the left
-- operand preserves the overall type of the expression. Dropping the
-- right operand would not, so we only mutate by dropping the LHS.
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "RemoveAction",
      operatorDescription = "Remove one non-binding action from a do block",
      operatorMatch = \le ->
        case viewThenChain le of
          Just (lhs, rest) ->
            Just (mutateChain (lhsExprType le) lhs rest)
          Nothing -> matchRawHsDo le
    }

-- | Try to view a typechecked expression as @lhs >> rest@.
--
-- The desugared form is @HsApp _ (HsApp _ ((>>) @m @a @b $d) lhs) rest@.
-- We use 'opOccName' to walk through type and dictionary applications and
-- check that the head is indeed @(>>)@.
viewThenChain ::
  LHsExpr GhcTc ->
  Maybe (LHsExpr GhcTc, LHsExpr GhcTc)
viewThenChain le = case unLoc le of
  XExpr (ExpandedThingTc _ expanded) -> viewThenChain (L (getLoc le) expanded)
  HsApp _ outer rhs -> case unLoc outer of
    HsApp _ f lhs
      | Just ">>" <- opOccName f -> Just (lhs, rhs)
    _ -> Nothing
  _ -> Nothing

-- | Build the mutation: replace @lhs >> rest@ with just @rest@.
--
-- Suppressed when @lhs@'s syntactic head is on the @ignore@ list: removing
-- e.g. @logDebug "x"@ from @do { logDebug "x"; rest }@ is exactly the noise
-- the ignore filter exists to silence.  The filter in 'instrumentLExpr'
-- cannot catch this on its own because GHC 9.10 has expanded the do-block
-- into @(>>) lhs rest@, whose head is @(>>)@, not the ignored name — so we
-- check the removed action's head here instead.  Returning @[]@ makes
-- 'applyOperator' treat the site as a non-candidate (no warning, no mutant).
mutateChain ::
  Type ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  InstrM [MutationAlt]
mutateChain ty lhs rest = do
  ignore <- asks instrumentEnvIgnore
  let lhsIgnored = case opOccName lhs of
        Just occ -> occ `elem` ignore
        Nothing -> False
  if lhsIgnored
    then pure []
    else
      let removedSpan = case getLocA lhs of
            RealSrcSpan rss _ -> [rss]
            UnhelpfulSpan _ -> []
       in pure
            [ MutationAlt
                { mutAltType = ty,
                  mutAltExpr = rest,
                  mutAltOriginal = "a >> rest",
                  mutAltReplacement = "rest",
                  mutAltDelta = SpanRemoval removedSpan,
                  mutAltMitigation = Nothing
                }
            ]

-- | Fallback: also support @HsDo@ if it ever shows up unexpanded
-- (e.g. list comprehensions, arrow notation), since the original
-- operator was written for that.
matchRawHsDo ::
  LHsExpr GhcTc ->
  Maybe (InstrM [MutationAlt])
matchRawHsDo = \case
  le@(L ann (HsDo x ctx (L lann stmts)))
    | any isRemovableStmt stmts ->
        Just (rawDoAction ann x ctx lann stmts (lhsExprType le))
  _ -> Nothing

isRemovableStmt :: ExprLStmt GhcTc -> Bool
isRemovableStmt (L _ s) = case s of
  BodyStmt {} -> True
  _ -> False

-- | The expression of a 'BodyStmt', if this statement is one.
bodyStmtExpr :: ExprLStmt GhcTc -> Maybe (LHsExpr GhcTc)
bodyStmtExpr (L _ s) = case s of
  BodyStmt _ e _ _ -> Just e
  _ -> Nothing

rawDoAction ::
  SrcSpanAnnA ->
  XDo GhcTc ->
  HsDoFlavour ->
  SrcSpanAnnL ->
  [ExprLStmt GhcTc] ->
  Type ->
  InstrM [MutationAlt]
rawDoAction ann x ctx lann stmts ty = do
  -- Do not offer to remove a statement whose head is on the 'ignore' list:
  -- removing @logDebug "x"@ and the like is the noise the ignore filter
  -- exists to silence.  Mirrors the guard in 'mutateChain' for the expanded
  -- @(>>)@ form.
  ignore <- asks instrumentEnvIgnore
  let stmtIgnored s = case bodyStmtExpr s >>= opOccName of
        Just occ -> occ `elem` ignore
        Nothing -> False
      removable = [(i, s) | (i, s) <- zip [0 ..] stmts, isRemovableStmt s, not (stmtIgnored s)]
      n = length stmts
      mkMutation i s =
        let stmts' = take i stmts ++ drop (i + 1) stmts
            removedSpan = case getLocA s of
              RealSrcSpan rss _ -> [rss]
              UnhelpfulSpan _ -> []
         in MutationAlt
              { mutAltType = ty,
                mutAltExpr = L ann (HsDo x ctx (L lann stmts')),
                mutAltOriginal = show n ++ " statements",
                mutAltReplacement = show (n - 1) ++ " statements (removed #" ++ show (i + 1) ++ ")",
                mutAltDelta = SpanRemoval removedSpan,
                mutAltMitigation = Nothing
              }
  pure [mkMutation i s | (i, s) <- removable]
