{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.ConstEmptyList (theOperator) where

import Control.Monad.Reader (asks)
import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (listTyCon)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), OpAppCtx (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (ConstFnMatch (..), arrowTy, mkConstLambda, prefixFormPreview, viewConstFnResult)

-- | Replace an expression whose type is @arg1 -> ... -> argN -> [a]@
-- (with @N >= 0@) with the constant function returning @[]@.
--
-- Same shape and rationale as 'ConstNothing'; see that module's haddock.
-- Complements 'ListLit', which targets the syntactic 'ExplicitList' form.
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "ConstEmptyList",
      operatorDescription = "Replace a list-typed expression (or a function returning a list) with a constant []",
      operatorMatch = \le ->
        case viewConstFnResult 0 listTyCon le of
          Just m@ConstFnMatch {cfnTyConArgs = [elTy]} -> Just (action le elTy m)
          _ -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  Type ->
  ConstFnMatch ->
  InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)]
action le elTy ConstFnMatch {cfnArgTys, cfnResTy} = do
  opAppCtx <- asks instrumentEnvOpAppCtx
  appDepth <- asks instrumentEnvAppDepth
  let arity = length cfnArgTys
  -- See 'ConstNothing' for the dominance rule.
  if arity >= 1 && appDepth >= arity
    then pure []
    else
      let emptyExpr = noLocA (ExplicitList elTy [])
          wholeTy = arrowTy cfnArgTys cfnResTy
          mutated = mkConstLambda cfnArgTys cfnResTy emptyExpr
          atOpToken = case (arity, opAppCtx, getLocA le) of
            (n, Just ctx, RealSrcSpan mSp _) | n >= 1, mSp == opAppOpSpan ctx -> Just ctx
            _ -> Nothing
          delta = case atOpToken of
            Just ctx ->
              ReplaceOuterSpan
                (opAppOuterSpan ctx)
                (prefixFormPreview arity "[]" (opAppLhsText ctx) (opAppRhsText ctx))
            Nothing ->
              let tokenText = case cfnArgTys of
                    [] -> "[]"
                    _ ->
                      "(\\"
                        <> T.replicate arity "_ "
                        <> "-> [])"
               in TokenReplace tokenText
          origLabel = case cfnArgTys of
            [] -> "e"
            _ -> "f"
          replLabel = case cfnArgTys of
            [] -> "[]"
            _ -> "\\" ++ unwords (replicate arity "_") ++ " -> []"
       in pure [(wholeTy, mutated, origLabel, replLabel, delta)]
