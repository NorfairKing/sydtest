{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.ConstNothing (theOperator) where

import Control.Monad.Reader (asks)
import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (maybeTyCon, nothingDataCon)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationAlt (..), MutationOperator (..), MutationOperatorKind (..), OpAppCtx (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (ConstFnMatch (..), arrowTy, mkConstLambda, prefixFormPreview, viewConstFnResult)

-- | Replace an expression whose type is @arg1 -> ... -> argN -> Maybe a@
-- (with @N >= 0@) with the constant function returning 'Nothing'.
--
--   * At arity 0, the mutant is just @Nothing@.
--   * At arity N \>= 1, it is @\\_ ... _ -> Nothing@ (typed at GhcTc as a
--     single 'HsLam' with N wildcard patterns).
--
-- Complements 'MaybeOp', which targets the syntactic @Just e@ form only.
-- The expression's outermost head must not be a data constructor (this op
-- never mutates @Just x@).
--
-- An arity-\>=1 firing is suppressed when 'instrumentEnvAppDepth' >= arity:
-- the matched expression is already saturated by enclosing applications,
-- so the arity-N mutant reduces to the same value the arity-0 mutation on
-- the enclosing saturated expression produces.  Emitting both is noise.
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "ConstNothing",
      operatorDescription = "Replace a Maybe-typed expression (or a function returning Maybe) with a constant Nothing",
      operatorKind = ExpressionOperator $ \le ->
        case viewConstFnResult 0 maybeTyCon le of
          Just m -> Just (action le m)
          Nothing -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  ConstFnMatch ->
  InstrM [MutationAlt]
action le ConstFnMatch {cfnArgTys, cfnResTy} = do
  opAppCtx <- asks instrumentEnvOpAppCtx
  appDepth <- asks instrumentEnvAppDepth
  let arity = length cfnArgTys
  if arity >= 1 && appDepth >= arity
    then pure []
    else
      let nothingExpr = nlHsDataCon nothingDataCon
          wholeTy = arrowTy cfnArgTys cfnResTy
          mutated = mkConstLambda cfnArgTys cfnResTy nothingExpr
          atOpToken = case (arity, opAppCtx, getLocA le) of
            (n, Just ctx, RealSrcSpan mSp _) | n >= 1, mSp == opAppOpSpan ctx -> Just ctx
            _ -> Nothing
          delta = case atOpToken of
            Just ctx ->
              ReplaceOuterSpan
                (opAppOuterSpan ctx)
                (prefixFormPreview arity "Nothing" (opAppLhsText ctx) (opAppRhsText ctx))
            Nothing ->
              let tokenText = case cfnArgTys of
                    [] -> "Nothing"
                    _ ->
                      "(\\"
                        <> T.replicate arity "_ "
                        <> "-> Nothing)"
               in TokenReplace tokenText
          origLabel = case cfnArgTys of
            [] -> "e"
            _ -> "f"
          replLabel = case cfnArgTys of
            [] -> "Nothing"
            _ -> "\\" ++ unwords (replicate arity "_") ++ " -> Nothing"
       in pure
            [ MutationAlt
                { mutAltType = wholeTy,
                  mutAltExpr = mutated,
                  mutAltOriginal = origLabel,
                  mutAltReplacement = replLabel,
                  mutAltDelta = delta,
                  mutAltMitigation = Nothing
                }
            ]
