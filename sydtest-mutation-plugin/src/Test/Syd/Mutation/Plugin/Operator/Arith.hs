{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.Arith (theOperator) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import GHC
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), SrcSpanDelta (..), liftTcM)
import Test.Syd.Mutation.Plugin.Operator.Util (TcOpApp (..), matchTcOpApp, mkOpReplacement)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "Arith",
      operatorDescription = "Replace any binary arithmetic operator with every other arithmetic operator",
      operatorMatch = \le -> case matchTcOpApp le of
        Just tcOp@(TcOpApp {tcOpAppOcc = occ})
          | occ `elem` arithOps ->
              Just (action tcOp)
        _ -> Nothing
    }

-- | The arithmetic operators we mutate between.
--
-- @(/)@ is intentionally excluded: it requires a @Fractional@ instance, so
-- substituting it for @(+)@ on, say, an @Int@ produces an ill-typed Core
-- term. Even when the typechecker happens to accept the substitution because
-- of how the plugin reuses the surrounding type and dictionary arguments,
-- the runtime behaviour is unpredictable (the dictionary is still the
-- original @Num@ dictionary, not the @Fractional@ one @(/)@ expects).
--
-- This means @Fractional@-typed expressions will not currently produce a
-- @\"/\"@ mutation. If we want @\"/\"@ as a mutation in the future, the
-- operator needs to check that the operand type is @Fractional@ and also
-- swap in the appropriate dictionary, which is not straightforward at the
-- typechecked AST level.
arithOps :: [String]
arithOps = ["+", "-", "*"]

action ::
  TcOpApp ->
  InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)]
action TcOpApp {tcOpAppTy, tcOpAppLhs, tcOpAppOp, tcOpAppRhs, tcOpAppOcc, tcOpAppOpSrcSpan} = do
  InstrumentEnv {instrumentEnvRdrEnv} <- ask
  let replacements = filter (/= tcOpAppOcc) arithOps
      delta replOcc = case tcOpAppOpSrcSpan of
        Just rss -> TokenReplaceAt rss (T.pack replOcc)
        Nothing -> TokenReplace (T.pack replOcc)
  mapM
    ( \replOcc -> do
        repl <- liftTcM $ mkOpReplacement instrumentEnvRdrEnv tcOpAppLhs tcOpAppOp tcOpAppRhs replOcc
        pure (tcOpAppTy, repl, tcOpAppOcc, replOcc, delta replOcc)
    )
    replacements
