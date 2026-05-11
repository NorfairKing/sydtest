{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.IntLit (theOperator) where

import Data.List.NonEmpty (NonEmpty (..))
import GHC
import GHC.Builtin.Types (naturalTy, word8Ty, wordTy)
import GHC.Core.TyCo.Compare (tcEqType)
import GHC.Core.Type (splitTyConApp_maybe)
import GHC.Types.Name (getOccString)
import GHC.Types.SourceText (il_value)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))
import Test.Syd.Mutation.Plugin.Operator.Util (mkIntLitReplacement)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "IntLit",
      operatorDescription = "Replace an integer literal n with 0, 1, and -n",
      operatorMatch = \case
        (L _ (HsOverLit _ (OverLit oltc@(OverLitTc {ol_type = ty}) (HsIntegral il)))) ->
          Just (action ty oltc (il_value il))
        _ -> Nothing
    }

-- | True for unsigned integral types where negating a positive literal would overflow.
isUnsignedIntegralTy :: Type -> Bool
isUnsignedIntegralTy ty =
  any (tcEqType ty) [word8Ty, wordTy, naturalTy]
    || case splitTyConApp_maybe ty of
      Just (tc, []) -> getOccString tc `elem` ["Word16", "Word32", "Word64"]
      _ -> False

action ::
  Type ->
  OverLitTc ->
  Integer ->
  InstrM (NonEmpty (Type, LHsExpr GhcTc, String, String))
action ty oltc n =
  let negCandidate = [negate n | n > 0 && not (isUnsignedIntegralTy ty)]
      candidates = filter (/= n) (0 : 1 : negCandidate)
      alts = map (\r -> (ty, mkIntLitReplacement r oltc, show n, show r)) candidates
   in case alts of
        (x : xs) -> pure (x :| xs)
        [] -> pure ((ty, mkIntLitReplacement 0 oltc, show n, "0") :| [])
