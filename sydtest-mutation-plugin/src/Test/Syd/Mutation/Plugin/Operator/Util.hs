{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.Util
  ( opOccName,
    mkOpReplacement,
    mkIntLitReplacement,
    lhsExprType,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import GHC
import GHC.HsToCore (deSugarExpr)
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.Env (tcLookupId)
import GHC.Tc.Utils.Monad (getTopEnv)
import GHC.Types.Error (isEmptyMessages)
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (lookupOccEnv, mkVarOcc)
import GHC.Types.Name.Reader (GlobalRdrEnv, greName)
import GHC.Types.SourceText (mkIntegralLit)

-- | Extract the OccName string of the operator in an 'OpApp' @op@ expression.
opOccName :: LHsExpr GhcTc -> Maybe String
opOccName = \case
  L _ (HsVar _ (L _ v)) -> Just (getOccString v)
  L _ (XExpr (WrapExpr (HsWrap _ e))) -> opOccName (noLocA e)
  _ -> Nothing

-- | Given the left operand, original @op@, and right operand of an 'OpApp',
-- look up @replOccStr@ in the module's rdr env and return a replacement
-- 'OpApp' expression with the new operator substituted in.
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
  pure (substOpInOpApp l replOp r origOp)
  where
    -- Replace the innermost HsVar id in the op expression, preserving wraps.
    substOpId :: Id -> LHsExpr GhcTc -> LHsExpr GhcTc
    substOpId newId = \case
      L ann (HsVar x (L l' _)) -> L ann (HsVar x (L l' newId))
      L ann (XExpr (WrapExpr (HsWrap w e))) ->
        L ann (XExpr (WrapExpr (HsWrap w (unLoc (substOpId newId (noLocA e))))))
      other -> other

    substOpInOpApp ::
      LHsExpr GhcTc ->
      LHsExpr GhcTc ->
      LHsExpr GhcTc ->
      LHsExpr GhcTc ->
      LHsExpr GhcTc
    substOpInOpApp l' op' r' (L ann (OpApp x _ _ _)) = L ann (OpApp x l' op' r')
    substOpInOpApp _ _ _ orig = orig

-- | Build a replacement integer literal, returning 'Nothing' if desugaring
-- the expression produces any diagnostics (e.g. -Woverflowed-literals for a
-- value outside the target type's range).
mkIntLitReplacement :: Integer -> OverLitTc -> TcM (Maybe (LHsExpr GhcTc))
mkIntLitReplacement n oltc = do
  let iln = mkIntegralLit n
      witnessN = substIntegerInWitness n (ol_witness oltc)
      oltcN = oltc {ol_witness = witnessN}
      expr = noLocA (HsOverLit NoExtField (OverLit oltcN (HsIntegral iln)))
  hscEnv <- getTopEnv
  (msgs, mcore) <- liftIO $ deSugarExpr hscEnv expr
  pure $ if isEmptyMessages msgs && isJust mcore then Just expr else Nothing

-- | Substitute the integer value in a @fromInteger dict (HsInteger _ n _)@ witness.
substIntegerInWitness :: Integer -> HsExpr GhcTc -> HsExpr GhcTc
substIntegerInWitness n = \case
  HsApp x f arg -> HsApp x f (fmap (substIntegerInWitness n) arg)
  HsLit x (HsInteger src _ ty) -> HsLit x (HsInteger src n ty)
  XExpr (WrapExpr (HsWrap w e)) -> XExpr (WrapExpr (HsWrap w (substIntegerInWitness n e)))
  e -> e

-- | Extract the result 'Type' of a type-checked expression.
-- Works for overloaded literals and wrapped expressions.
lhsExprType :: LHsExpr GhcTc -> Maybe Type
lhsExprType = go . unLoc
  where
    go = \case
      HsOverLit _ (OverLit (OverLitTc {ol_type = ty}) _) -> Just ty
      XExpr (WrapExpr (HsWrap _ e)) -> go e
      HsVar _ (L _ v) -> Just (idType v)
      _ -> Nothing
