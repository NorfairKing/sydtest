{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.IntLit1To0 (theOperator) where

import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Types.SourceText (il_value)
import Test.Syd.Mutation.Plugin.Instrument (MutationOperator (..))
import Test.Syd.Mutation.Plugin.Operator.Util (mkIntLitReplacement)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "IntLit1To0",
      operatorDescription = "Replace a literal 1 by a literal 0",
      operatorMatch = \case
        (L _ (HsOverLit _ (OverLit oltc@(OverLitTc {ol_type = ty}) (HsIntegral il))))
          | il_value il == 1 ->
              Just (pure ((ty, mkIntLitReplacement 0 oltc, "1", "0") :| []))
        _ -> Nothing
    }
