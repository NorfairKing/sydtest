{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.Util
  ( opOccName,
    mkOpReplacement,
    mkIntLitReplacement,
    lhsExprType,
  )
where

import Control.Monad.IO.Class (liftIO)
import GHC
import GHC.Core.Type (splitTyConApp_maybe)
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.Env (tcLookupId)
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

-- | Build a replacement integer literal, returning 'Nothing' if @n@ does not
-- fit in the boxed type of the literal (e.g. -1 for a Word8 literal).
mkIntLitReplacement :: Integer -> OverLitTc -> Maybe (LHsExpr GhcTc)
mkIntLitReplacement n oltc
  | integerFitsInBoxedTy (ol_type oltc) n =
      let iln = mkIntegralLit n
          witnessN = substIntegerInWitness n (ol_witness oltc)
          oltcN = oltc {ol_witness = witnessN}
       in Just (noLocA (HsOverLit NoExtField (OverLit oltcN (HsIntegral iln))))
  | otherwise = Nothing

-- | Substitute the integer value in a @fromInteger dict (HsInteger _ n _)@ witness.
substIntegerInWitness :: Integer -> HsExpr GhcTc -> HsExpr GhcTc
substIntegerInWitness n = \case
  HsApp x f arg -> HsApp x f (fmap (substIntegerInWitness n) arg)
  HsLit x (HsInteger src _ ty) -> HsLit x (HsInteger src n ty)
  XExpr (WrapExpr (HsWrap w e)) -> XExpr (WrapExpr (HsWrap w (substIntegerInWitness n e)))
  e -> e

-- | True if @n@ fits in the value range of the given boxed integer type.
-- Identified by TyCon name; returns True for unrecognised types (conservative).
integerFitsInBoxedTy :: Type -> Integer -> Bool
integerFitsInBoxedTy ty n = case splitTyConApp_maybe ty of
  Just (tc, []) -> case getOccString tc of
    "Int" -> n >= toInteger (minBound :: Int) && n <= toInteger (maxBound :: Int)
    "Word" -> n >= 0 && n <= toInteger (maxBound :: Word)
    "Int8" -> n >= -128 && n <= 127
    "Word8" -> n >= 0 && n <= 255
    "Int16" -> n >= -32768 && n <= 32767
    "Word16" -> n >= 0 && n <= 65535
    "Int32" -> n >= -2147483648 && n <= 2147483647
    "Word32" -> n >= 0 && n <= 4294967295
    "Int64" -> n >= -9223372036854775808 && n <= 9223372036854775807
    "Word64" -> n >= 0 && n <= 18446744073709551615
    "Natural" -> n >= 0
    _ -> True
  _ -> True

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
