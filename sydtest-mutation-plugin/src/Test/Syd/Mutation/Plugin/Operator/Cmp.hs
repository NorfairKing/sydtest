{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.Cmp (theOperator) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (boolTy)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), SrcSpanDelta (..), liftTcM)
import Test.Syd.Mutation.Plugin.Operator.Util (TcOpApp (..), matchTcOpApp, mkOpReplacement)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "Cmp",
      operatorDescription = "Replace a comparison operator with another in the same class",
      operatorMatch = \le -> case matchTcOpApp le of
        Just tcOp@(TcOpApp {tcOpAppOcc = occ})
          | not (null (replacementsFor occ)) ->
              Just (action tcOp (replacementsFor occ))
        _ -> Nothing
    }

-- | Mutation substitutions are restricted to the 'Ord' operators
-- (@<@, @<=@, @>@, @>=@), which all use an 'Ord' dictionary and so can be
-- substituted for each other while preserving the original dictionary
-- argument.
--
-- @==@/@/=@ are excluded for two reasons:
--
--   1. Crossing into the 'Ord' class would call an 'Ord' method through an
--      'Eq' dictionary (or vice versa), which crashes GHC's code generator
--      (@Prelude.!!: index too large@) at best and produces undefined
--      behaviour at worst.
--   2. Within 'Eq' itself, substituting @==@ for @/=@ on primitive types
--      like 'Int' does not actually change the runtime behaviour. The
--      class-method selector is resolved to the instance method ('eqInt' or
--      'neInt') during desugaring in a way that the post-typecheck @HsVar@
--      substitution does not influence. The mutation fires and the manifest
--      records it, but the test would not be able to distinguish the
--      original from the mutant. See task follow-up for a proper fix.
ordOps :: [String]
ordOps = ["<", "<=", ">", ">="]

-- | Operators that should be considered as alternatives for @occ@: those in
-- the same class, minus @occ@ itself. Returns @[]@ if @occ@ is not a known
-- comparison operator.
replacementsFor :: String -> [String]
replacementsFor occ
  | occ `elem` ordOps = filter (/= occ) ordOps
  | otherwise = []

action ::
  TcOpApp ->
  [String] ->
  InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)]
action TcOpApp {tcOpAppLhs, tcOpAppOp, tcOpAppRhs, tcOpAppOcc, tcOpAppOpSrcSpan} replacements = do
  InstrumentEnv {instrRdrEnv} <- ask
  let delta replOcc = case tcOpAppOpSrcSpan of
        Just rss -> TokenReplaceAt rss (T.pack replOcc)
        Nothing -> TokenReplace (T.pack replOcc)
  mapM
    ( \replOcc -> do
        repl <- liftTcM $ mkOpReplacement instrRdrEnv tcOpAppLhs tcOpAppOp tcOpAppRhs replOcc
        pure (boolTy, repl, tcOpAppOcc, replOcc, delta replOcc)
    )
    replacements
