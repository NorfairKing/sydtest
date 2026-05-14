{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.Util
  ( opOccName,
    mkOpReplacement,
    mkIntLitExpr,
    lhsExprType,
    TcOpApp (..),
    matchTcOpApp,
  )
where

import Control.Monad.IO.Class (liftIO)
import GHC
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.Env (tcLookupId)
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (lookupOccEnv, mkVarOcc)
import GHC.Types.Name.Reader (GlobalRdrEnv, greName)
import GHC.Types.SourceText (mkIntegralLit)

-- | Extract the OccName string of the operator at the head of an application
-- chain.  At GhcTc the operator often has type and dictionary arguments
-- attached (e.g. @(+) \@Int $dNumInt@), so we walk through 'HsApp',
-- 'HsAppType', and 'WrapExpr' wrappers to find the underlying 'HsVar'.
opOccName :: LHsExpr GhcTc -> Maybe String
opOccName = \case
  L _ (HsVar _ (L _ v)) -> Just (getOccString v)
  L _ (XExpr (WrapExpr (HsWrap _ e))) -> opOccName (noLocA e)
  L _ (HsApp _ f _) -> opOccName f
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
      Just occ <- opOccName tcOp,
      Just ty <- lhsExprType tcL ->
        Just (TcOpApp ty tcL tcOp tcR occ (origOpSrcSpan orig))
  -- Fallback for any compiler version that keeps OpApp at GhcTc.
  OpApp _ lhs op rhs
    | Just occ <- opOccName op,
      Just ty <- lhsExprType lhs ->
        Just (TcOpApp ty lhs op rhs occ (rnOpSrcSpan op))
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
      inner = noLocA (HsApp NoExtField replOp l)
      outer = noLocA (HsApp NoExtField inner r)
  pure outer
  where
    -- Replace the innermost HsVar id in the op expression, preserving wraps.
    substOpId :: Id -> LHsExpr GhcTc -> LHsExpr GhcTc
    substOpId newId = \case
      L ann (HsVar x (L l' _)) -> L ann (HsVar x (L l' newId))
      L ann (XExpr (WrapExpr (HsWrap w e))) ->
        L ann (XExpr (WrapExpr (HsWrap w (unLoc (substOpId newId (noLocA e))))))
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

-- | Extract the result 'Type' of a type-checked expression.
-- Walks through 'WrapExpr', 'ExpandedThingTc', 'HsPar', 'HsApp' and
-- 'HsAppType' to find an expression whose type we can read directly.
lhsExprType :: LHsExpr GhcTc -> Maybe Type
lhsExprType = go . unLoc
  where
    go = \case
      HsOverLit _ (OverLit (OverLitTc {ol_type = ty}) _) -> Just ty
      HsVar _ (L _ v) -> Just (idType v)
      HsPar _ e -> go (unLoc e)
      XExpr (WrapExpr (HsWrap _ e)) -> go e
      -- After typecheck, many source forms become @ExpandedThingTc orig expanded@.
      -- The @expanded@ field carries the real typechecked expression we can
      -- read the result type from.
      XExpr (ExpandedThingTc _ expanded) -> go expanded
      -- For an application @f x@, the result type is the application's type,
      -- which (post type/dict args) we can read from the head's idType after
      -- stripping the type arrows. As a simple approximation, walk into the
      -- function position: this matches the typechecker's own approach.
      _ -> Nothing
