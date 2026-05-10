{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.IntLit1To0 (theOperator) where

import GHC
import GHC.Types.SourceText (il_value, mkIntegralLit)
import Test.Syd.Mutation.Plugin.Instrument (MutationOperator (..))

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "IntLit1To0",
      operatorDescription = "Replace a literal 1 by a literal 0",
      operatorMatch = \case
        (L _ (HsOverLit _ (OverLit oltc@(OverLitTc {ol_type = ty}) (HsIntegral il))))
          | il_value il == 1 ->
              let il0 = mkIntegralLit (0 :: Integer)
                  witness0 = substIntegerInWitness 0 (ol_witness oltc)
                  oltc0 = oltc {ol_witness = witness0}
                  zero = noLocA (HsOverLit NoExtField (OverLit oltc0 (HsIntegral il0)))
               in Just (ty, zero, "1", "0")
        _ -> Nothing
    }

-- | Substitute the Integer value in a @fromInteger dict (HsInteger _ n _)@ witness.
substIntegerInWitness :: Integer -> HsExpr GhcTc -> HsExpr GhcTc
substIntegerInWitness n = \case
  HsApp x f arg -> HsApp x f (fmap (substIntegerInWitness n) arg)
  HsLit x (HsInteger src _ ty) -> HsLit x (HsInteger src n ty)
  -- HsWrap wraps the fromInteger application with a coercion.
  XExpr (WrapExpr (HsWrap w e)) -> XExpr (WrapExpr (HsWrap w (substIntegerInWitness n e)))
  e -> e
