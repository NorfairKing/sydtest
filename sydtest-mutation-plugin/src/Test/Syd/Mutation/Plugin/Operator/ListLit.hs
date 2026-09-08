{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.ListLit (theOperator) where

import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import GHC
import GHC.Builtin.Types (mkListTy)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationAlt (..), MutationOperator (..), MutationOperatorKind (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (nameMatchCandidates)
import Test.Syd.Mutation.Plugin.OptParse (OperatorConfig (..), operatorExtraStrings)

-- | Shrink a list literal by removing elements or emptying it.
--
-- A list whose elements are the pieces of a message is a list nobody asserts
-- the elements of, and every such literal is one more mutant that can only be
-- killed by pinning wording. Those are named by the call they are an argument
-- of, under the operator's @skip-calls-to@ config key:
--
-- > operators:
-- >   ListLit:
-- >     skip-calls-to:
-- >       - logInfo
-- >       - fail
--
-- Any enclosing call counts, not only the immediate one, because the immediate
-- one is @mconcat@ or @unwords@ for every message built out of pieces: in
-- @fail $ mconcat [\"unknown: \", x]@ it is @fail@ that says what the list is
-- for, and it is two applications out. @$@ is seen through for the same
-- reason.
--
-- A name matches either bare (@fail@, matching any module) or fully qualified
-- (@GHC.Internal.Base.fail@). A qualifier may be either the function's
-- defining module or a module it is imported through.
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "ListLit",
      operatorDescription = "Shrink a list literal by removing elements or emptying it",
      operatorKind = ExpressionOperator $ \case
        (L ann (ExplicitList elTy es))
          | length es >= 2 ->
              Just (action ann elTy es)
        _ -> Nothing
    }

action ::
  SrcSpanAnnA ->
  Type ->
  [LHsExpr GhcTc] ->
  InstrM [MutationAlt]
action ann elTy es = do
  opsConfig <- asks instrumentEnvOperatorsConfig
  rdrEnv <- asks instrumentEnvRdrEnv
  enclosing <- asks instrumentEnvEnclosingCalls
  let extra = maybe Map.empty operatorConfigExtra (Map.lookup "ListLit" opsConfig)
      skipCallsTo = operatorExtraStrings "skip-calls-to" extra
      skipThisList =
        not (null skipCallsTo)
          && any
            (\n -> any (`elem` skipCallsTo) (nameMatchCandidates rdrEnv n))
            enclosing
  if skipThisList
    then pure []
    else
      let listTy = mkListTy elTy
          n = length es
          toRss e = case getLocA e of
            RealSrcSpan rss _ -> [rss]
            UnhelpfulSpan _ -> []
          mkList xs delta =
            MutationAlt
              { mutAltType = listTy,
                mutAltExpr = L ann (ExplicitList elTy xs),
                mutAltOriginal = show n ++ " elements",
                mutAltReplacement = show (length xs) ++ " elements",
                mutAltDelta = delta,
                mutAltMitigation = Nothing
              }
          -- Always produce: empty list, drop-head.
          -- Only add drop-last if it gives a different length than drop-head
          -- (i.e. n > 2; when n == 2 both give one element).
          lastE = reverse es
          repls = case es of
            [] -> []
            (firstE : restEs) ->
              mkList [] (SpanRemoval (concatMap toRss es))
                : mkList restEs (SpanRemoval (toRss firstE))
                : case lastE of
                  [] -> []
                  (le : _) -> [mkList (take (n - 1) es) (SpanRemoval (toRss le)) | n > 2]
       in pure repls
