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
import GHC.Builtin.Types.Prim
import GHC.Core.TyCo.Compare (tcEqType)
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
-- fit in the primitive type of the witness (e.g. -1 in a Word8 witness).
mkIntLitReplacement :: Integer -> OverLitTc -> Maybe (LHsExpr GhcTc)
mkIntLitReplacement n oltc =
  case substIntegerInWitness n (ol_witness oltc) of
    Nothing -> Nothing
    Just witnessN ->
      let iln = mkIntegralLit n
          oltcN = oltc {ol_witness = witnessN}
       in Just (noLocA (HsOverLit NoExtField (OverLit oltcN (HsIntegral iln))))

-- | Substitute the integer value in a @fromInteger dict (HsInteger _ n _)@
-- witness, returning 'Nothing' if @n@ is out of range for the primitive type.
substIntegerInWitness :: Integer -> HsExpr GhcTc -> Maybe (HsExpr GhcTc)
substIntegerInWitness n = \case
  HsApp x f arg ->
    HsApp x f <$> traverse (substIntegerInWitness n) arg
  HsLit x (HsInteger src _ primTy)
    | integerFitsInPrimTy primTy n -> Just (HsLit x (HsInteger src n primTy))
    | otherwise -> Nothing
  XExpr (WrapExpr (HsWrap w e)) ->
    XExpr . WrapExpr . HsWrap w <$> substIntegerInWitness n e
  e -> Just e

-- | True if @n@ is in the valid range for the given GHC primitive integer type.
-- Returns True for any unrecognised type (conservative: allow the mutation).
integerFitsInPrimTy :: Type -> Integer -> Bool
integerFitsInPrimTy primTy n
  | eq intPrimTy = n >= toInteger (minBound :: Int) && n <= toInteger (maxBound :: Int)
  | eq wordPrimTy = n >= 0 && n <= toInteger (maxBound :: Word)
  | eq int8PrimTy = n >= -128 && n <= 127
  | eq word8PrimTy = n >= 0 && n <= 255
  | eq int16PrimTy = n >= -32768 && n <= 32767
  | eq word16PrimTy = n >= 0 && n <= 65535
  | eq int32PrimTy = n >= -2147483648 && n <= 2147483647
  | eq word32PrimTy = n >= 0 && n <= 4294967295
  | eq int64PrimTy = n >= -9223372036854775808 && n <= 9223372036854775807
  | eq word64PrimTy = n >= 0 && n <= 18446744073709551615
  | otherwise = True
  where
    eq = tcEqType primTy

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
