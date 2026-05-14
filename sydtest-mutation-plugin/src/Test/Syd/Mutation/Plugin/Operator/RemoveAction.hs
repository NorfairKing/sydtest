{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.RemoveAction (theOperator) where

import GHC
import GHC.Hs.Syn.Type (lhsExprType)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..), SrcSpanDelta (..))
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
mutateChain ::
  Type ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)]
mutateChain ty lhs rest =
  let removedSpan = case getLocA lhs of
        RealSrcSpan rss _ -> [rss]
        UnhelpfulSpan _ -> []
   in pure
        [ ( ty,
            rest,
            "a >> rest",
            "rest",
            SpanRemoval removedSpan
          )
        ]

-- | Fallback: also support @HsDo@ if it ever shows up unexpanded
-- (e.g. list comprehensions, arrow notation), since the original
-- operator was written for that.
matchRawHsDo ::
  LHsExpr GhcTc ->
  Maybe (InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)])
matchRawHsDo = \case
  le@(L ann (HsDo x ctx (L lann stmts)))
    | any isRemovableStmt stmts ->
        Just (rawDoAction ann x ctx lann stmts (lhsExprType le))
  _ -> Nothing

isRemovableStmt :: ExprLStmt GhcTc -> Bool
isRemovableStmt (L _ s) = case s of
  BodyStmt {} -> True
  _ -> False

rawDoAction ::
  SrcSpanAnnA ->
  XDo GhcTc ->
  HsDoFlavour ->
  SrcSpanAnnL ->
  [ExprLStmt GhcTc] ->
  Type ->
  InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)]
rawDoAction ann x ctx lann stmts ty =
  let removable = [(i, s) | (i, s) <- zip [0 ..] stmts, isRemovableStmt s]
      n = length stmts
      mkMutation i s =
        let stmts' = take i stmts ++ drop (i + 1) stmts
            removedSpan = case getLocA s of
              RealSrcSpan rss _ -> [rss]
              UnhelpfulSpan _ -> []
         in ( ty,
              L ann (HsDo x ctx (L lann stmts')),
              show n ++ " statements",
              show (n - 1) ++ " statements (removed #" ++ show (i + 1) ++ ")",
              SpanRemoval removedSpan
            )
   in pure [mkMutation i s | (i, s) <- removable]
