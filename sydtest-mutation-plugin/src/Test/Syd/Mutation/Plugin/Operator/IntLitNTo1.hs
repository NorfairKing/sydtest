{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.IntLitNTo1 (theOperator) where

import GHC
import GHC.Types.SourceText (il_value)
import Test.Syd.Mutation.Plugin.Instrument (MutationOperator (..))
import Test.Syd.Mutation.Plugin.Operator.Util (mkIntLitReplacement)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "IntLitNTo1",
      operatorDescription = "Replace a non-one integer literal by 1",
      operatorMatch = \case
        (L _ (HsOverLit _ (OverLit oltc@(OverLitTc {ol_type = ty}) (HsIntegral il))))
          | il_value il /= 1 ->
              Just (pure (ty, mkIntLitReplacement 1 oltc, show (il_value il), "1"))
        _ -> Nothing
    }
