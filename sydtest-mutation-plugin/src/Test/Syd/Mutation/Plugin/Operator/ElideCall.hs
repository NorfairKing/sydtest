{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.Operator.ElideCall (theOperator) where

import Control.Monad.Reader (asks)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC
import GHC.Core.TyCo.Compare (eqType)
import GHC.Hs.Syn.Type (lhsExprType)
import GHC.Types.Name (getOccString)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationAlt (..), MutationOperator (..), MutationOperatorKind (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (collectApp, headFunctionName, nameMatchCandidates, spanText)
import Test.Syd.Mutation.Plugin.OptParse (OperatorConfig (..), operatorExtraStrings)

-- | Replace a function application @f a@ with its argument @a@ whenever the
-- argument's type equals the result type of the whole application.
--
-- The application node already has the type the surrounding context expects,
-- so requiring the argument to have that same type is both necessary and
-- sufficient for the elided expression to keep type-checking.  This catches
-- calls that are supposed to /do something/ to a value of a type they also
-- return: @abs x@, @reverse xs@, @sort xs@, @succ n@, @T.strip t@, @id x@.  A
-- test that does not observe the difference between @f a@ and @a@ leaves the
-- mutant alive.
--
-- The condition makes the operator naturally selective:
--
--   * Data constructors never fire (@Just a@ has type @Maybe A /= A@, and the
--     same holds for tuples, @(:)@, and newtype constructors under nominal
--     equality), so this does not overlap 'MaybeOp' or 'ListLit'.
--   * Type and dictionary applications never fire: @f \@Int@ is an 'HsAppType',
--     not an 'HsApp', and a dictionary argument has a predicate type that
--     cannot equal the result type.
--
-- Only prefix applications are mutated.  At GhcTc an infix application @x + y@
-- is the expanded chain @(+) x y@ inside an @ExpandedThingTc@; the walker
-- recurses into that expansion with 'instrumentExpr' rather than
-- 'instrumentLExpr', so the saturated outer node is never offered to operators,
-- and the inner partial application @(+) x@ has a function result type that
-- fails the type-fit test.  Infix operators are therefore left to their own
-- operators ('Arith', 'Cmp', 'LogicOp').
--
-- Each application node is mutated independently, so for @g (f a)@ both
-- @g (f a) -> f a@ and @f a -> a@ are produced (each when its own type fits);
-- the two mutants have distinct spans, so unlike 'SwitchFunctionArguments'
-- this needs no application-depth guard against duplicates.
--
-- @id x@ is an /equivalent/ mutant — @id x@ and @x@ are the same value, so no
-- test can kill it — and so are other identity-on-this-type functions.  Calls
-- to @id@ are skipped by default; further functions can be suppressed by name
-- under the operator's @skip-calls-to@ config key:
--
-- > operators:
-- >   ElideCall:
-- >     skip-calls-to:
-- >       - fromIntegral
--
-- A name matches either bare (@fromIntegral@) or fully qualified
-- (@GHC.Real.fromIntegral@).
theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "ElideCall",
      operatorDescription = "Replace a function application with its argument when the argument's type matches the result type",
      operatorKind = ExpressionOperator $ \le -> case unLoc le of
        HsApp _ _ arg
          | eqType (lhsExprType arg) (lhsExprType le) ->
              Just (action le arg)
        _ -> Nothing
    }

action ::
  LHsExpr GhcTc ->
  LHsExpr GhcTc ->
  InstrM [MutationAlt]
action le arg = do
  srcLines <- asks (maybe [] snd . instrumentEnvSourceFile)
  opsConfig <- asks instrumentEnvOperatorsConfig
  let extra = maybe Map.empty operatorConfigExtra (Map.lookup "ElideCall" opsConfig)
      -- @id@ is always an equivalent (unkillable) mutant, so it is skipped by
      -- default; other identity-like functions can be added by the user.
      skipCallsTo = "id" : operatorExtraStrings "skip-calls-to" extra
      (headExpr, _) = collectApp le
      skipThisCall = case headFunctionName headExpr of
        Just n -> any (`elem` skipCallsTo) (nameMatchCandidates n)
        Nothing -> False
  case (skipThisCall, getLocA le, getLocA arg) of
    (False, RealSrcSpan callSp _, RealSrcSpan argSp _)
      | not (null srcLines) ->
          let argText = spanText srcLines argSp
              callText = spanText srcLines callSp
           in pure
                [ MutationAlt
                    { mutAltType = lhsExprType le,
                      mutAltExpr = arg,
                      mutAltOriginal = T.unpack callText,
                      mutAltReplacement = T.unpack argText,
                      mutAltDelta = TokenReplace argText,
                      mutAltMitigation = mitigationFor headExpr
                    }
                ]
    _ -> pure []

-- | The mitigation hint recorded on every elision: if the called function is
-- an identity on this argument the mutant is equivalent (unkillable), and
-- listing the function under @skip-calls-to@ suppresses it.  'Nothing' when the
-- head is not a named function (so there is no name to suggest).
mitigationFor :: LHsExpr GhcTc -> Maybe Text
mitigationFor headExpr = do
  n <- headFunctionName headExpr
  let fn = getOccString n
  pure $
    T.pack $
      concat
        [ "If `",
          fn,
          "` is an identity on this argument this is an equivalent mutant ",
          "that no test can kill; add `",
          fn,
          "` to this operator's `skip-calls-to` config to suppress it."
        ]
