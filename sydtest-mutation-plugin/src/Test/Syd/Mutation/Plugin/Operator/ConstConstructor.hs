{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.ConstConstructor (theOperator) where

import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (boolTyCon, listTyCon, maybeTyCon)
import GHC.Core (CoreExpr, Expr (..), isTyCoArg, maybeUnfoldingTemplate)
import GHC.Core.ConLike (ConLike (RealDataCon))
import GHC.Core.DataCon (dataConFullSig, dataConWrapId)
import GHC.Core.TyCon (tyConDataCons_maybe)
import GHC.Core.Type (splitTyConApp_maybe)
import GHC.Types.Id (isDataConId_maybe, realIdUnfolding)
import GHC.Types.Name (getOccString)
import GHC.Types.Name.Occurrence (isSymOcc, occNameString)
import GHC.Types.Var (isTyVar)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationAlt (..), MutationOperator (..), MutationOperatorKind (..), OpAppCtx (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (ConstFnMatch (..), ConstructorHeads (..), arrowTy, collectApp, headFunctionName, mkConstLambda, nameMatchCandidates, prefixFormPreview, viewConstFnResultBy)
import Test.Syd.Mutation.Plugin.OptParse (OperatorConfig (..), operatorExtraStrings)

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
-- A function whose result is always the same constructor produces an
-- /equivalent/ mutant at every call to it: replacing the call with that
-- constructor cannot change anything, so no test can kill it.  A delegating
-- @sqlType Proxy = sqlType (Proxy :: Proxy Text)@ is the shape that keeps
-- coming up -- @sqlType@ answers with a constant of its type by definition.
-- Which functions those are is a semantic property the plugin cannot detect,
-- so calls to them are suppressed by listing the function's name under the
-- operator's @skip-calls-to@ config key:
--
-- > operators:
-- >   ConstConstructor:
-- >     skip-calls-to:
-- >       - sqlType
--
-- A name matches either bare (@sqlType@, matching any module) or fully
-- qualified (@Database.Persist.Sql.sqlType@), by the defining module or by a
-- module it is imported through.  This is the same matching the @ignore@ key
-- and the other operators' @skip-calls-to@ keys use.
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
  -- Suppress calls to functions the user has marked constant-valued (this
  -- operator's mutants for them are equivalent and unkillable).  See this
  -- module's haddock.
  opsConfig <- asks instrumentEnvOperatorsConfig
  rdrEnv <- asks instrumentEnvRdrEnv
  let extra = maybe Map.empty operatorConfigExtra (Map.lookup "ConstConstructor" opsConfig)
  let skipCallsTo = operatorExtraStrings "skip-calls-to" extra
  let skipThisCall = case headFunctionName (fst (collectApp le)) of
        Just n -> any (`elem` skipCallsTo) (nameMatchCandidates rdrEnv n)
        Nothing -> False
  let arity = length cfnArgTys
  -- See 'ConstNothing' for the dominance rule.
  if (arity >= 1 && appDepth >= arity) || skipThisCall
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
                    mutAltMitigation = mitigationFor le headCon
                  }
       in pure [mkAlt dc | dc <- cons, Just dc /= headCon]

-- | Hint shown for a surviving mutation: if the called function always
-- answers with the same constructor the mutant is equivalent (unkillable),
-- and listing the function under @skip-calls-to@ suppresses it.  'Nothing'
-- when the expression is already a constructor, or its head is not a named
-- function, so there is nothing to suggest.
mitigationFor :: LHsExpr GhcTc -> Maybe DataCon -> Maybe Text
mitigationFor _ (Just _) = Nothing
mitigationFor le Nothing = do
  n <- headFunctionName (fst (collectApp le))
  let fn = getOccString n
  pure $
    T.pack $
      concat
        [ "If `",
          fn,
          "` always returns the same constructor this is an equivalent mutant ",
          "that no test can kill; add `",
          fn,
          "` to this operator's `skip-calls-to` config to suppress it."
        ]

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
--
-- A variable counts when it is a constructor itself and also when it is a
-- binding defined as one; see 'aliasedConstructor'.
constructorHead :: LHsExpr GhcTc -> Maybe DataCon
constructorHead = \case
  L _ (XExpr (ConLikeTc (RealDataCon dc) _ _)) -> Just dc
  L _ (HsVar _ (L _ v)) -> case isDataConId_maybe v of
    Just dc -> Just dc
    Nothing -> aliasedConstructor v
  L _ (HsApp _ f _) -> constructorHead f
  L _ (HsAppType _ f _) -> constructorHead f
  L _ (HsPar _ e) -> constructorHead e
  L _ (ExprWithTySig _ e _) -> constructorHead e
  L _ (XExpr (WrapExpr (HsWrap _ e))) -> constructorHead (noLocA e)
  L _ (XExpr (ExpandedThingTc _ e)) -> constructorHead (noLocA e)
  _ -> Nothing

-- | The nullary constructor a binding is an alias for, read off the unfolding
-- GHC recorded for it.
--
-- @Data.Map.Strict.empty@ is @Tip@: an ordinary function whose entire
-- definition is a nullary constructor.  Replacing an occurrence of it with
-- that constructor is exactly the no-op this operator already declines to
-- offer when the constructor is written out, only reached through a name, so
-- it has to be recognised here or every @Map.empty@ in a codebase becomes an
-- unkillable mutant.  The same holds for @Set.empty@, @Seq.empty@, and any
-- @emptyFoo = NoFoo@ of the user's own.
--
-- Best-effort: only an imported binding has an unfolding at this stage, and
-- only when its defining module recorded one.  A miss costs a no-op mutant,
-- which is what would be produced without this check at all.
--
-- Reading the unfolding at all takes the plugin unsetting
-- @-fignore-interface-pragmas@, which @-O0@ implies and instrumented builds
-- are compiled at; see the driver plugin in "Test.Syd.Mutation.Plugin".
aliasedConstructor :: Id -> Maybe DataCon
aliasedConstructor v = do
  template <- maybeUnfoldingTemplate (realIdUnfolding v)
  coreConstructorHead template

-- | The constructor a Core expression is a bare occurrence of.
--
-- Type abstractions and type applications are peeled because a constructor of
-- a parameterised type reaches its use site under them (@empty@ unfolds to
-- @\\\@k \\\@a -> Tip \@k \@a@).  A value argument is not peeled: it means the
-- constructor is applied to something and so is not a constant of its type.
coreConstructorHead :: CoreExpr -> Maybe DataCon
coreConstructorHead = \case
  Var i -> isDataConId_maybe i
  App f a | isTyCoArg a -> coreConstructorHead f
  Lam b e | isTyVar b -> coreConstructorHead e
  Cast e _ -> coreConstructorHead e
  Tick _ e -> coreConstructorHead e
  _ -> Nothing
