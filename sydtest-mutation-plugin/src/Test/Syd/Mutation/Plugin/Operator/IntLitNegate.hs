{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.IntLitNegate (theOperator) where

import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Types.SourceText (il_value)
import Test.Syd.Mutation.Plugin.Instrument (MutationOperator (..))
import Test.Syd.Mutation.Plugin.Operator.Util (mkIntLitReplacement)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "IntLitNegate",
      operatorDescription = "Replace an integer literal n by -n",
      operatorMatch = \case
        (L _ (HsOverLit _ (OverLit oltc@(OverLitTc {ol_type = ty}) (HsIntegral il))))
          | il_value il /= 0 ->
              let n = il_value il
                  neg = negate n
               in Just (pure ((ty, mkIntLitReplacement neg oltc, show n, show neg) :| []))
        _ -> Nothing
    }
