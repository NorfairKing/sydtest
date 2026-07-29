{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.ConstConstructor (theOperator) where

import Control.Monad.Reader (asks)
import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (boolTyCon, listTyCon, maybeTyCon)
import GHC.Core.ConLike (ConLike (RealDataCon))
import GHC.Core.DataCon (dataConFullSig, dataConWrapId)
import GHC.Core.TyCon (tyConDataCons_maybe)
import GHC.Core.Type (splitTyConApp_maybe)
import GHC.Types.Id (isDataConId_maybe)
import GHC.Types.Name.Occurrence (isSymOcc, occNameString)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationAlt (..), MutationOperator (..), MutationOperatorKind (..), OpAppCtx (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (ConstFnMatch (..), ConstructorHeads (..), arrowTy, mkConstLambda, prefixFormPreview, viewConstFnResultBy)

-- | Replace an expression whose type is @arg1 -> ... -> argN -> T tys@ (with
-- @N >= 0@) with a constant function returning a nullary constructor of @T@,
-- one mutant per such constructor.
--
--   * At arity 0, the mutants are the bare constructors.
--   * At arity N \>= 1, they are @\\_ ... _ -> Con@, typed at GhcTc via
--     'mkConstLambda' — the same shape 'ConstBool' and 'ConstNothing' use.
--
-- A nullary constructor is a constant of its type, so it type-checks wherever
-- a value of that type is expected.  That covers both an enumeration
-- (@data ABC = A | B | C@, where every constructor is a constant, so every
-- value can be switched to every other) and a type that merely has one
-- (@data MyMaybe a = MyNothing | MyJust a@, where @MyNothing@ is the only
-- replacement offered) — the latter being the user-defined-type counterpart
-- of what 'ConstNothing' and 'MaybeOp' do for 'Maybe'.
--
-- Four restrictions keep the mutant set free of duplicates and no-ops:
--
--   * 'Bool', 'Maybe' and lists are excluded.  @Nothing@ and @[]@ are nullary
--     constructors like any other, but 'ConstBool' and 'BoolLit',
--     'ConstNothing' and 'MaybeOp', and 'ConstEmptyList' and 'ListLit'
--     already produce exactly these mutants for those three types.
--   * A type with fewer than two constructors is excluded: every value of it
--     is built from the same constructor, so no replacement can change
--     anything.  (This also rules out @()@ and tuples.)
--   * A constructor is a candidate replacement only when it takes no
--     arguments /and/ its signature has no existentials, no constraints, and
--     no GADT equality: otherwise it is not a constant, or not one at the
--     type this site needs.  This is GHC's own @is_enum_con@ test, applied
--     per constructor rather than to the whole type.
--   * Unlike the rest of the @Const…@ family this operator matches
--     constructor-headed expressions, since for a user-defined type no other
--     operator claims them.  It drops the alternative that replaces a
--     constructor with itself, which would be an unkillable no-op.
--
-- An arity-\>=1 firing is suppressed when 'instrumentEnvAppDepth' >= arity;
-- see 'ConstNothing' for that dominance rule.
--
-- The manifest preview names the constructor unqualified even when the
-- mutated module does not have it in scope.  The mutant itself is built from
-- the constructor's 'Id' and compiles regardless of scope, so this only
-- affects how the diff reads.
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "ConstConstructor",
      operatorDescription = "Replace an expression (or a function's result) with a constant constructor of its type",
      operatorKind = ExpressionOperator $ \le -> do
        m <- viewConstFnResultBy AllowConstructorHeads 0 hasConstantDataCons le
        cons <- constantDataConsOfType (cfnResTy m)
        pure (action le m cons)
    }

action ::
  LHsExpr GhcTc ->
  ConstFnMatch ->
  [DataCon] ->
  InstrM [MutationAlt]
action le ConstFnMatch {cfnArgTys, cfnResTy, cfnTyConArgs} cons = do
  opAppCtx <- asks instrumentEnvOpAppCtx
  appDepth <- asks instrumentEnvAppDepth
  let arity = length cfnArgTys
  -- See 'ConstNothing' for the dominance rule.
  if arity >= 1 && appDepth >= arity
    then pure []
    else
      let wholeTy = arrowTy cfnArgTys cfnResTy
          -- The constructor this expression is already built from, if any.
          -- Replacing it with itself is dropped below.
          headCon = constructorHead le
          atOpToken = case (arity, opAppCtx, getLocA le) of
            (n, Just ctx, RealSrcSpan mSp _) | n >= 1, mSp == opAppOpSpan ctx -> Just ctx
            _ -> Nothing
          mkAlt dc =
            let conName = conSourceName dc
                -- Instantiating the constructor's type arguments matters for
                -- a parameterised type (@data MyMaybe a = MyNothing |
                -- MyJust a@): the bare constructor is @forall a. MyMaybe a@,
                -- whose 'forall' would make the surrounding @ifMutation \@ty@
                -- wrapper ill-typed.  See 'mkNothingExpr' for what that
                -- miscompiles into.
                v = nlHsTyApp (dataConWrapId dc) cfnTyConArgs
                mutated = mkConstLambda cfnArgTys cfnResTy v
                delta = case atOpToken of
                  Just ctx ->
                    ReplaceOuterSpan
                      (opAppOuterSpan ctx)
                      (prefixFormPreview arity (T.pack conName) (opAppLhsText ctx) (opAppRhsText ctx))
                  Nothing ->
                    let tokenText = case cfnArgTys of
                          [] -> T.pack conName
                          _ -> T.concat ["(\\", T.replicate arity "_ ", "-> ", T.pack conName, ")"]
                     in TokenReplace tokenText
                origLabel = case cfnArgTys of
                  [] -> maybe "e" conSourceName headCon
                  _ -> "f"
                replLabel = case cfnArgTys of
                  [] -> conName
                  _ -> "\\" ++ unwords (replicate arity "_") ++ " -> " ++ conName
             in MutationAlt
                  { mutAltType = wholeTy,
                    mutAltExpr = mutated,
                    mutAltOriginal = origLabel,
                    mutAltReplacement = replLabel,
                    mutAltDelta = delta,
                    mutAltMitigation = Nothing
                  }
       in pure [mkAlt dc | dc <- cons, Just dc /= headCon]

-- | How the constructor is written in an expression: a symbolic constructor
-- like @(:<)@ needs its parentheses to be one.
conSourceName :: DataCon -> String
conSourceName dc =
  let occ = getOccName dc
   in if isSymOcc occ
        then concat ["(", occNameString occ, ")"]
        else occNameString occ

-- | Whether the const-family matcher should accept a result type headed by
-- this TyCon.
hasConstantDataCons :: TyCon -> Bool
hasConstantDataCons tc = case constantDataCons tc of
  Just _ -> True
  Nothing -> False

-- | The constructors to mutate to, for a result type accepted by
-- 'hasConstantDataCons'.
constantDataConsOfType :: Type -> Maybe [DataCon]
constantDataConsOfType ty = do
  (tc, _) <- splitTyConApp_maybe ty
  constantDataCons tc

-- | The constant constructors of a TyCon this operator handles, or 'Nothing'
-- when it handles none of them.
constantDataCons :: TyCon -> Maybe [DataCon]
constantDataCons tc
  | tc `elem` [boolTyCon, maybeTyCon, listTyCon] = Nothing
  | otherwise = do
      cons <- tyConDataCons_maybe tc
      case cons of
        (_ : _ : _) -> case filter isConstantDataCon cons of
          [] -> Nothing
          constants -> Just constants
        _ -> Nothing

-- | Whether a constructor is a constant of @T tys@ for every @tys@: it takes
-- no arguments, and its signature does not refine the type with
-- existentials, constraints, or a GADT equality, so applying the type
-- arguments of the site being mutated builds a value of exactly that type.
isConstantDataCon :: DataCon -> Bool
isConstantDataCon dc =
  let (_univTvs, exTvs, eqSpec, theta, argTys, _resTy) = dataConFullSig dc
   in null exTvs && null eqSpec && null theta && null argTys

-- | The data constructor an expression is built from, if it is a constructor
-- application.  Peels the wrappers the typechecker leaves around a
-- constructor occurrence, mirroring
-- 'Test.Syd.Mutation.Plugin.Operator.Util.nonConstructorHead'.
constructorHead :: LHsExpr GhcTc -> Maybe DataCon
constructorHead = \case
  L _ (XExpr (ConLikeTc (RealDataCon dc) _ _)) -> Just dc
  L _ (HsVar _ (L _ v)) -> isDataConId_maybe v
  L _ (HsApp _ f _) -> constructorHead f
  L _ (HsAppType _ f _) -> constructorHead f
  L _ (HsPar _ e) -> constructorHead e
  L _ (ExprWithTySig _ e _) -> constructorHead e
  L _ (XExpr (WrapExpr (HsWrap _ e))) -> constructorHead (noLocA e)
  L _ (XExpr (ExpandedThingTc _ e)) -> constructorHead (noLocA e)
  _ -> Nothing
