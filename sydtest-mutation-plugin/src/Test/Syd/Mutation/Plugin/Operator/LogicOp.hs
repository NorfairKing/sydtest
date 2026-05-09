{-# LANGUAGE NamedFieldPuns #-}

module Test.Syd.Mutation.Plugin.Operator.LogicOp (theOperator) where

import Control.Monad.Reader (ask)
import qualified Data.Text as T
import GHC
import GHC.Builtin.Types (boolTy)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, InstrumentEnv (..), MutationOperator (..), SrcSpanDelta (..), liftTcM)
import Test.Syd.Mutation.Plugin.Operator.Util (TcOpApp (..), matchTcOpApp, mkOpReplacement)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "LogicOp",
      operatorDescription = "Replace a boolean binary operator with the other",
      operatorMatch = \le -> case matchTcOpApp le of
        Just tcOp@(TcOpApp {tcOpAppOcc = occ})
          | occ `elem` logicOps ->
              Just (action tcOp)
        _ -> Nothing
    }

logicOps :: [String]
logicOps = ["&&", "||"]

action ::
  TcOpApp ->
  InstrM [(Type, LHsExpr GhcTc, String, String, SrcSpanDelta)]
action TcOpApp {tcOpAppLhs, tcOpAppOp, tcOpAppRhs, tcOpAppOcc, tcOpAppOpSrcSpan} = do
  InstrumentEnv {instrRdrEnv} <- ask
  let replacements = filter (/= tcOpAppOcc) logicOps
      delta replOcc = case tcOpAppOpSrcSpan of
        Just rss -> TokenReplaceAt rss (T.pack replOcc)
        Nothing -> TokenReplace (T.pack replOcc)
  mapM
    ( \replOcc -> do
        repl <- liftTcM $ mkOpReplacement instrRdrEnv tcOpAppLhs tcOpAppOp tcOpAppRhs replOcc
        pure (boolTy, repl, tcOpAppOcc, replOcc, delta replOcc)
    )
    replacements
