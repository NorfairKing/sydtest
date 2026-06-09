{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.Util
  ( opOccName,
    mkOpReplacement,
    mkIntLitExpr,
    TcOpApp (..),
    matchTcOpApp,
    ConstFnMatch (..),
    viewConstFnResult,
    mkConstLambda,
    arrowTy,
    prefixFormPreview,
    unwrapWrap,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (manyDataConTy)
import GHC.Core.TyCo.Rep (Scaled (..))
import GHC.Core.Type (isForAllTy, mkVisFunTyMany, splitFunTy_maybe, splitTyConApp_maybe)
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.Env (tcLookupId)
import GHC.Types.Basic (DoPmc (..), GenReason (..), Origin (..))
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (lookupOccEnv, mkVarOcc)
import GHC.Types.Name.Reader (GlobalRdrEnv, greName)
import GHC.Types.SourceText (mkIntegralLit)

-- | Extract the OccName string of the operator at the head of an application
-- chain.  At GhcTc the operator often has type and dictionary arguments
-- attached (e.g. @(+) \@Int $dNumInt@), so we walk through 'HsApp',
-- 'HsAppType', and 'WrapExpr' wrappers to find the underlying 'HsVar'.
--
-- Also peels through @XExpr (ExpandedThingTc orig expanded)@: GHC 9.6+
-- expands source-level @f $ x@ and similar rebindable-syntax forms into
-- @HsApp@ chains wrapped in 'ExpandedThingTc'.  Without this case, a call
-- like @logInfo $ ...@ would look like a "no head" expression, and the
-- ignore filter (which dispatches on 'opOccName') would let mutations
-- leak into the argument subtree.
--
-- Treats @($)@ specially: at GhcTc, @f $ x@ is expanded into the application
-- chain @($) f x@, but the syntactic "head" the user thinks of is @f@, not
-- @($)@.  Whenever we encounter @($) f x@ (or any further nesting of @$@s),
-- we recurse on the first argument instead of returning @"$"@.
opOccName :: LHsExpr GhcTc -> Maybe String
opOccName = \case
  L _ (HsVar _ (L _ v)) -> Just (getOccString v)
  L _ (XExpr (WrapExpr (HsWrap _ e))) -> opOccName (noLocA e)
  L _ (XExpr (ExpandedThingTc _ e)) -> opOccName (noLocA e)
  L _ (HsApp _ f a)
    | Just "$" <- opOccName f -> opOccName a
    | otherwise -> opOccName f
  L _ (HsAppType _ f _) -> opOccName f
  L _ (HsPar _ e) -> opOccName e
  _ -> Nothing

-- | A view of a binary infix operator application as it appears in the
-- typechecked AST.  At GhcTc, GHC always expands the source-level @OpApp@ into
-- @HsApp (HsApp op lhs) rhs@ wrapped in @XExpr (ExpandedThingTc (OrigExpr orig)
-- expanded)@.  Operators that want to match infix uses (Arith, Cmp, LogicOp)
-- should use 'matchTcOpApp' rather than pattern-matching on @OpApp@ directly,
-- which would always fail.
data TcOpApp = TcOpApp
  { -- | The result type of the operator application.
    tcOpAppTy :: Type,
    -- | The left operand (typechecked).
    tcOpAppLhs :: LHsExpr GhcTc,
    -- | The operator expression (typechecked, with type and dictionary args
    -- still attached).  Used as the substitution target for
    -- 'mkOpReplacement'.
    tcOpAppOp :: LHsExpr GhcTc,
    -- | The right operand (typechecked).
    tcOpAppRhs :: LHsExpr GhcTc,
    -- | The operator's @OccName@ string (e.g. \"+\", \"==\").
    tcOpAppOcc :: String,
    -- | The source 'RealSrcSpan' of the operator token (as written by the
    -- user), if available.  Recovered from the renamed @OrigExpr@ inside
    -- @ExpandedThingTc@; will be @Nothing@ if the operator originated outside
    -- a syntactic infix application.
    tcOpAppOpSrcSpan :: Maybe RealSrcSpan
  }

-- | Match a typechecked binary infix operator application.  Recognises both
-- the post-typecheck expanded form (@ExpandedThingTc@ wrapping an @HsApp@
-- chain) and a raw @OpApp@ as a fallback.
matchTcOpApp :: LHsExpr GhcTc -> Maybe TcOpApp
matchTcOpApp le = case unLoc le of
  -- Post-typecheck expanded form: @ExpandedThingTc (OrigExpr (OpApp _ l op r))
  --                                                (HsApp _ (HsApp _ tcOp tcL) tcR)@.
  XExpr (ExpandedThingTc orig expanded)
    | Just (tcOp, tcL, tcR) <- viewHsApp2 expanded,
      Just occ <- opOccName tcOp ->
        Just (TcOpApp (lhsExprType tcL) tcL tcOp tcR occ (origOpSrcSpan orig))
  -- Fallback for any compiler version that keeps OpApp at GhcTc.
  OpApp _ lhs op rhs
    | Just occ <- opOccName op ->
        Just (TcOpApp (lhsExprType lhs) lhs op rhs occ (rnOpSrcSpan op))
  _ -> Nothing

-- | Get the source 'RealSrcSpan' of the operator from the renamed origin of
-- an @ExpandedThingTc@ wrapper, if it was an infix application.
origOpSrcSpan :: HsThingRn -> Maybe RealSrcSpan
origOpSrcSpan = \case
  OrigExpr (OpApp _ _ op _) -> rnOpSrcSpan op
  _ -> Nothing

-- | Get the 'RealSrcSpan' of an operator expression (renamed or typechecked).
rnOpSrcSpan :: GenLocated SrcSpanAnnA e -> Maybe RealSrcSpan
rnOpSrcSpan (L l _) = case locA l of
  RealSrcSpan rss _ -> Just rss
  UnhelpfulSpan _ -> Nothing

-- | Decompose a 2-argument application: @HsApp _ (HsApp _ f a) b@ → @(f, a, b)@.
-- Skips through 'WrapExpr' wrappers.
viewHsApp2 :: HsExpr GhcTc -> Maybe (LHsExpr GhcTc, LHsExpr GhcTc, LHsExpr GhcTc)
viewHsApp2 e0 = case unwrapWrap e0 of
  HsApp _ outer rhs -> case unwrapWrap (unLoc outer) of
    HsApp _ f lhs -> Just (f, lhs, rhs)
    _ -> Nothing
  _ -> Nothing

-- | Skip outermost 'WrapExpr' layers.
unwrapWrap :: HsExpr GhcTc -> HsExpr GhcTc
unwrapWrap = \case
  XExpr (WrapExpr (HsWrap _ inner)) -> unwrapWrap inner
  e -> e

-- | Build the mutated infix application by substituting the operator id.
--
-- @origOp@ is the typechecked operator expression (an @HsVar@ wrapped in type
-- and dictionary applications).  We replace the innermost 'HsVar' id with the
-- replacement operator's id, then reconstruct the application as
-- @HsApp _ (HsApp _ origOp[op := replOp] lhs) rhs@.
mkOpReplacement ::
  GlobalRdrEnv ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  String ->
  TcM (LHsExpr GhcTc)
mkOpReplacement rdrEnv l origOp r replOccStr = do
  replId <- case lookupOccEnv rdrEnv (mkVarOcc replOccStr) of
    Just (gre : _) -> tcLookupId (greName gre)
    _ ->
      liftIO $
        ioError $
          userError $
            "mutation: replacement operator " ++ replOccStr ++ " not in scope"
  let replOp = substOpId replId origOp
      inner = mkHsApp replOp l
      outer = mkHsApp inner r
  pure outer
  where
    -- Replace the innermost HsVar id in the op expression, preserving the
    -- surrounding 'HsApp', 'HsAppType', 'HsPar', and 'WrapExpr' wrappers that
    -- carry the typechecker's type and dictionary applications. Mirrors the
    -- structure 'opOccName' walks: substOpId must reach the same 'HsVar' that
    -- 'opOccName' would have returned the OccName for.
    substOpId :: Id -> LHsExpr GhcTc -> LHsExpr GhcTc
    substOpId newId = \case
      L ann (HsVar x (L l' _)) -> L ann (HsVar x (L l' newId))
      L ann (XExpr (WrapExpr (HsWrap w e))) ->
        L ann (XExpr (WrapExpr (HsWrap w (unLoc (substOpId newId (noLocA e))))))
      L ann (HsApp x f a) -> L ann (HsApp x (substOpId newId f) a)
      L ann (HsAppType x f t) -> L ann (HsAppType x (substOpId newId f) t)
      L ann (HsPar x e) -> L ann (HsPar x (substOpId newId e))
      other -> other

-- | Build a replacement integer literal expression without any validation.
mkIntLitExpr :: Integer -> OverLitTc -> LHsExpr GhcTc
mkIntLitExpr n oltc =
  let iln = mkIntegralLit n
      witnessN = substIntegerInWitness n (ol_witness oltc)
      oltcN = oltc {ol_witness = witnessN}
   in noLocA (HsOverLit NoExtField (OverLit oltcN (HsIntegral iln)))

-- | Substitute the integer value in a @fromInteger dict (HsInteger _ n _)@ witness.
substIntegerInWitness :: Integer -> HsExpr GhcTc -> HsExpr GhcTc
substIntegerInWitness n = \case
  HsApp x f arg -> HsApp x f (fmap (substIntegerInWitness n) arg)
  HsLit x (HsInteger src _ ty) -> HsLit x (HsInteger src n ty)
  XExpr (WrapExpr (HsWrap w e)) -> XExpr (WrapExpr (HsWrap w (substIntegerInWitness n e)))
  e -> e

-- | Result of matching an expression for the @Const…@ family of operators:
-- peel arrows on @le@'s type until the head TyCon matches a target, with
-- the argument types of the arrows that were peeled.
data ConstFnMatch = ConstFnMatch
  { -- | Argument types of the arrows that were peeled, in order.  Empty for
    -- arity 0 (the expression itself already has the target type).
    cfnArgTys :: [Type],
    -- | The result type (the target TyCon applied to its arguments).
    cfnResTy :: Type,
    -- | The result type's TyCon arguments, e.g. @[a]@ for @[a] -> Maybe a@.
    cfnTyConArgs :: [Type]
  }

-- | If @le@'s type matches @arg1 -> ... -> argN -> targetTyCon tcArgs@ with
-- @N >= minArity@, return the peeled arrow argument types, the final result
-- type, and the target TyCon's arguments.  Used by the @Const…@ family.
--
-- Returns 'Nothing' when:
--
--   * @le@'s outermost head is a data constructor (e.g. @Just x@, @x : xs@) —
--     those have their own dedicated operators ('MaybeOp', 'ListLit') and
--     would duplicate them,
--   * the expression's type has a forall or class constraint (we can't
--     synthesise a constant under one without building a typed dictionary or
--     type lambda),
--   * after peeling arrows, the result type does not split as
--     @targetTyCon args@,
--   * the arity (number of arrows peeled) is less than @minArity@.
viewConstFnResult :: Int -> TyCon -> LHsExpr GhcTc -> Maybe ConstFnMatch
viewConstFnResult minArity targetTyCon le = do
  () <- nonConstructorHead le
  let ty = lhsExprType le
  if isForAllTy ty
    then Nothing
    else do
      let (argTys, resTy) = peelArrows ty
      if length argTys < minArity
        then Nothing
        else do
          (tc, tcArgs) <- splitTyConApp_maybe resTy
          if tc == targetTyCon
            then Just (ConstFnMatch argTys resTy tcArgs)
            else Nothing

-- | Peel as many @arg -> rest@ arrows as possible from the type, returning
-- the argument types and the final result type.  Stops at any
-- non-arrow type (including constrained types).
peelArrows :: Type -> ([Type], Type)
peelArrows = go []
  where
    go acc ty = case splitFunTy_maybe ty of
      Just (_af, _mult, argTy, resTy) -> go (argTy : acc) resTy
      Nothing -> (reverse acc, ty)

-- | Reject expressions whose outermost head is a data constructor at GhcTc
-- ('ConLikeTc' / a 'HsConLikeOut'-style node).  Peels through 'HsApp',
-- 'HsAppType', 'HsPar', 'WrapExpr', and 'ExpandedThingTc' wrappers.
nonConstructorHead :: LHsExpr GhcTc -> Maybe ()
nonConstructorHead = go
  where
    go h = case unLoc h of
      XExpr (ConLikeTc {}) -> Nothing
      HsApp _ f _ -> go f
      HsAppType _ f _ -> go f
      HsPar _ e -> go e
      XExpr (WrapExpr (HsWrap _ e)) -> go (L (getLoc h) e)
      XExpr (ExpandedThingTc _ e) -> go (L (getLoc h) e)
      _ -> Just ()

-- | Build @\\_ _ ... _ -> v@ at GhcTc.  @v@ must already have type @resTy@;
-- the resulting lambda has type @arg1 -> ... -> argN -> resTy@.
--
-- For arity 0 (empty @argTys@) this is the identity on @v@.
--
-- Used by the @Const…@ family of operators to produce a constant-function
-- mutant without depending on @Prelude.const@ being in scope.  All
-- annotation/origin fields are filled with neutral defaults so the desugarer
-- accepts the result and the pattern-match checker does not complain about
-- the wildcards.
mkConstLambda ::
  -- | Argument types, in source order: @[arg1, arg2, ..., argN]@.
  [Type] ->
  -- | Result type of @v@.
  Type ->
  -- | The bare value to wrap.
  LHsExpr GhcTc ->
  LHsExpr GhcTc
mkConstLambda [] _ body = body
mkConstLambda argTys resTy body =
  let pats = map (\ty -> noLocA (WildPat ty)) argTys
      grhs = noLocA (GRHS noAnn [] body)
      grhss = GRHSs emptyComments [grhs] (EmptyLocalBinds NoExtField)
      ctxt = LamAlt LamSingle
      match = noLocA (Match [] ctxt pats grhss)
      mgtc =
        MatchGroupTc
          { mg_arg_tys = map (Scaled manyDataConTy) argTys,
            mg_res_ty = resTy,
            mg_origin = Generated OtherExpansion SkipPmc
          }
      mg = MG mgtc (noLocA [match])
   in noLocA (HsLam [] LamSingle mg)

-- | Build an arrow type @arg1 -> ... -> argN -> resTy@ with default (Many)
-- multiplicity.
arrowTy :: [Type] -> Type -> Type
arrowTy as resTy = foldr mkVisFunTyMany resTy as

-- | Build a prefix-form preview text for an arity-N constant-function
-- mutation that sits at an infix operator position.
--
-- Given operand source text and a mutant rendering, produces text that
-- replaces the whole enclosing @OpApp@ source span in prefix form:
--
--   arity 2:  @(\\_ _ -> v) (lhsText) (rhsText)@
--   arity 1:  @(\\_ -> v) (rhsText)@           — the partial app
--                                              consumed the LHS already.
--
-- The result reparses as a normal Haskell expression and has the same
-- runtime semantics as the AST mutation, so the manifest diff is honest.
prefixFormPreview ::
  -- | Arity of the constant function being inserted.
  Int ->
  -- | Source text of the constant body (e.g. @"True"@, @"Nothing"@, @"[]"@).
  Text ->
  -- | Source text of the LHS operand of the infix @OpApp@.
  Text ->
  -- | Source text of the RHS operand of the infix @OpApp@.
  Text ->
  Text
prefixFormPreview arity vText lhsText rhsText =
  let lam =
        "(\\"
          <> T.replicate arity "_ "
          <> "-> "
          <> vText
          <> ")"
   in case arity of
        2 -> lam <> " (" <> lhsText <> ") (" <> rhsText <> ")"
        1 -> lam <> " (" <> rhsText <> ")"
        _ -> lam
