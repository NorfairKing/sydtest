{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.IntLitNTo0 (theOperator) where

import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Types.SourceText (il_value)
import Test.Syd.Mutation.Plugin.Instrument (MutationOperator (..))
import Test.Syd.Mutation.Plugin.Operator.Util (mkIntLitReplacement)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "IntLitNTo0",
      operatorDescription = "Replace a nonzero integer literal by 0",
      operatorMatch = \case
        (L _ (HsOverLit _ (OverLit oltc@(OverLitTc {ol_type = ty}) (HsIntegral il))))
          | il_value il /= 0 ->
              Just (pure ((ty, mkIntLitReplacement 0 oltc, show (il_value il), "0") :| []))
        _ -> Nothing
    }
