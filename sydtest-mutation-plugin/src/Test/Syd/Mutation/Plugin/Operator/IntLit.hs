{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.IntLit (theOperator) where

import qualified Data.Text as T
import GHC
import GHC.Types.SourceText (il_value)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationAlt (..), MutationOperator (..), MutationOperatorKind (..), SrcSpanDelta (..))
import Test.Syd.Mutation.Plugin.Operator.Util (mkIntLitExpr)

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "IntLit",
      operatorDescription = "Replace an integer literal n with 0, 1, and -n",
      operatorKind = ExpressionOperator $ \case
        (L _ (HsOverLit _ (OverLit oltc@(OverLitTc {ol_type = ty}) (HsIntegral il)))) ->
          Just (action ty oltc (il_value il))
        _ -> Nothing
    }

action ::
  Type ->
  OverLitTc ->
  Integer ->
  InstrM [MutationAlt]
action ty oltc n =
  let candidates = filter (/= n) [0, 1, negate n]
   in pure $
        map
          ( \r ->
              MutationAlt
                { mutAltType = ty,
                  mutAltExpr = mkIntLitExpr r oltc,
                  mutAltOriginal = show n,
                  mutAltReplacement = show r,
                  mutAltDelta = TokenReplace (T.pack (show r)),
                  mutAltMitigation = Nothing
                }
          )
          candidates
