{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.ConstBool (theOperator) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (boolTyCon, falseDataCon, trueDataCon)
import GHC.Types.Name (getOccString)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationAlt (..), MutationOperator (..), OpAppCtx (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (ConstFnMatch (..), arrowTy, mkConstLambda, prefixFormPreview, viewConstFnResult)

-- | Replace an expression whose type is @arg1 -> ... -> argN -> Bool@
-- (with @N >= 0@) with the constant function returning 'True' or 'False'.
--
--   * At arity 0, the mutants are bare @True@ and @False@.
--   * At arity N \>= 1, they are @\\_ ... _ -> True@ and
--     @\\_ ... _ -> False@ (typed at GhcTc via 'mkConstLambda').
--
-- Skips bare boolean literals 'True', 'False', and 'otherwise' at arity 0,
-- so the mutant set is disjoint from the original.  Inside a guard
-- expression (a 'BodyStmt' in a 'GRHS') the @False@ alternative is dropped:
-- mutating every guard to 'False' produces non-exhaustive matches that
-- throw an uncatchable 'MatchFail' at runtime.
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "ConstBool",
      operatorDescription = "Replace a Bool-typed expression (or a function returning Bool) with a constant True or False",
      operatorMatch = \le ->
        case viewConstFnResult 0 boolTyCon le of
          Just m
            | not (isArity0BoolLit m le) -> Just (action le m)
          _ -> Nothing
    }

-- | At arity 0, skip @True@, @False@, and @otherwise@.  Arity \>= 1 has no
-- such notion of "literal" — bare 'not' or '(&&)' are honest mutation
-- targets.
isArity0BoolLit :: ConstFnMatch -> LHsExpr GhcTc -> Bool
isArity0BoolLit ConstFnMatch {cfnArgTys} le = case cfnArgTys of
  [] -> isBoolLit le
  _ -> False

-- | Returns True for boolean literals and 'otherwise' (which equals True),
-- unwrapping HsWrap and ConLikeTc nodes that GHC inserts after type-checking.
isBoolLit :: LHsExpr GhcTc -> Bool
isBoolLit = \case
  L _ (HsVar _ (L _ v)) -> getOccString v `elem` ["True", "False", "otherwise"]
  L _ (XExpr (ConLikeTc con _ _)) -> getOccString con `elem` ["True", "False"]
  L _ (XExpr (WrapExpr (HsWrap _ e))) -> isBoolLit (noLocA e)
  _ -> False

action ::
  LHsExpr GhcTc ->
  ConstFnMatch ->
  InstrM [MutationAlt]
action le ConstFnMatch {cfnArgTys, cfnResTy} = do
  InstrumentEnv {instrumentEnvInGuard, instrumentEnvOpAppCtx, instrumentEnvAppDepth} <- ask
  let arity = length cfnArgTys
  -- See 'ConstNothing' for the dominance rule.
  if arity >= 1 && instrumentEnvAppDepth >= arity
    then pure []
    else
      let wholeTy = arrowTy cfnArgTys cfnResTy
          atOpToken = case (arity, instrumentEnvOpAppCtx, getLocA le) of
            (n, Just ctx, RealSrcSpan mSp _) | n >= 1, mSp == opAppOpSpan ctx -> Just ctx
            _ -> Nothing
          mkAlt boolCon boolName =
            let v = nlHsDataCon boolCon
                mutated = mkConstLambda cfnArgTys cfnResTy v
                delta = case atOpToken of
                  Just ctx ->
                    ReplaceOuterSpan
                      (opAppOuterSpan ctx)
                      (prefixFormPreview arity (T.pack boolName) (opAppLhsText ctx) (opAppRhsText ctx))
                  Nothing ->
                    let tokenText = case cfnArgTys of
                          [] -> T.pack boolName
                          _ ->
                            "(\\"
                              <> T.replicate arity "_ "
                              <> "-> "
                              <> T.pack boolName
                              <> ")"
                     in TokenReplace tokenText
                (origLabel, replLabel) = case cfnArgTys of
                  [] -> ("e", boolName)
                  _ -> ("f", "\\" ++ unwords (replicate arity "_") ++ " -> " ++ boolName)
             in MutationAlt
                  { mutAltType = wholeTy,
                    mutAltExpr = mutated,
                    mutAltOriginal = origLabel,
                    mutAltReplacement = replLabel,
                    mutAltDelta = delta,
                    mutAltMitigation = Nothing
                  }
          trueAlt = mkAlt trueDataCon "True"
          falseAlt = mkAlt falseDataCon "False"
          -- The guard carveout applies only at arity 0; a Bool-typed lambda is
          -- never itself the body of a 'BodyStmt' guard.
          dropFalse = instrumentEnvInGuard && null cfnArgTys
       in pure $ if dropFalse then [trueAlt] else [trueAlt, falseAlt]
