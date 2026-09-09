{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.ConstEmptyList (theOperator) where

import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (charTyCon, listTyCon)
import GHC.Core.Type (tyConAppTyCon_maybe)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationAlt (..), MutationOperator (..), MutationOperatorKind (..), OpAppCtx (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (ConstFnMatch (..), arrowTy, mkConstLambda, nameMatchCandidates, prefixFormPreview, unwrapWrap, viewConstFnResult)
import Test.Syd.Mutation.Plugin.OptParse (OperatorConfig (..), operatorExtraFlag, operatorExtraStrings)

-- | Replace an expression whose type is @arg1 -> ... -> argN -> [a]@
-- (with @N >= 0@) with the constant function returning @[]@.
--
-- Same shape and rationale as 'ConstNothing'; see that module's haddock.
-- Complements 'ListLit', which targets the syntactic 'ExplicitList' form.
--
-- It complements it in @skip-calls-to@ too, with the same meaning and the same
-- matching: a list holding the pieces of a message is a list nobody asserts
-- the elements of, and emptying it is a mutant that can only be killed by
-- pinning wording. Naming the calls that take a message reaches those lists
-- wherever they are written, including inside an instance method, which is
-- where an @ANN@ pragma cannot go.
--
-- > operators:
-- >   ConstEmptyList:
-- >     skip-calls-to:
-- >       - fail
--
-- 'ListLit' documents why any enclosing call counts rather than only the
-- immediate one.
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "ConstEmptyList",
      operatorDescription = "Replace a list-typed expression (or a function returning a list) with a constant []",
      operatorKind = ExpressionOperator $ \le ->
        case viewConstFnResult 0 listTyCon le of
          Just m@ConstFnMatch {cfnTyConArgs = [elTy]} -> Just (action le elTy m)
          _ -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  Type ->
  ConstFnMatch ->
  InstrM [MutationAlt]
action le elTy ConstFnMatch {cfnArgTys, cfnResTy} = do
  opAppCtx <- asks instrumentEnvOpAppCtx
  appDepth <- asks instrumentEnvAppDepth
  -- This operator interprets its own config entry's extra keys.
  opsConfig <- asks instrumentEnvOperatorsConfig
  rdrEnv <- asks instrumentEnvRdrEnv
  enclosing <- asks instrumentEnvEnclosingCalls
  let extra = maybe Map.empty operatorConfigExtra (Map.lookup "ConstEmptyList" opsConfig)
      skipStrings = operatorExtraFlag "skip-strings" extra
      skipLiteralStrings = operatorExtraFlag "skip-literal-strings" extra
      skipCallsTo = operatorExtraStrings "skip-calls-to" extra
      skippedByCall =
        not (null skipCallsTo)
          && any
            (\n -> any (`elem` skipCallsTo) (nameMatchCandidates rdrEnv n))
            enclosing
      arity = length cfnArgTys
      -- 'skip-strings' drops every @[Char]@-typed expression; the narrower
      -- 'skip-literal-strings' drops only syntactic string literals.
      skippedAsString =
        (skipStrings && isCharElemTy elTy)
          || (skipLiteralStrings && isCharElemTy elTy && isStringLiteralExpr le)
  -- See 'ConstNothing' for the dominance rule.
  if (arity >= 1 && appDepth >= arity) || skippedAsString || skippedByCall
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

-- | Whether the list element type is 'Char' (i.e. the list is a
-- @[Char]@/'String').  Used to honour @skip-strings@.
isCharElemTy :: Type -> Bool
isCharElemTy ty = tyConAppTyCon_maybe ty == Just charTyCon

-- | Whether the expression is (syntactically) a string literal.  Strips
-- evidence/parens wrappers and recognises the @fromString \"...\"@ form
-- that @OverloadedStrings@ produces.  Used to honour
-- @skip-literal-strings@.
isStringLiteralExpr :: LHsExpr GhcTc -> Bool
isStringLiteralExpr = go . unLoc
  where
    go e = case unwrapWrap e of
      HsLit _ HsString {} -> True
      HsPar _ inner -> go (unLoc inner)
      -- @OverloadedStrings@: @fromString "..."@.
      HsApp _ _ arg -> case unwrapWrap (unLoc arg) of
        HsLit _ HsString {} -> True
        _ -> False
      _ -> False
