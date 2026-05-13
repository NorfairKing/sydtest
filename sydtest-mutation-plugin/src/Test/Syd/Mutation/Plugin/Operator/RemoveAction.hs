{-# LANGUAGE LambdaCase #-}

module Test.Syd.Mutation.Plugin.Operator.RemoveAction (theOperator) where

import qualified Data.Text as T
import GHC
import GHC.Hs.Syn.Type (lhsExprType)
import Test.Syd.Mutation.Plugin.Instrument (InstrM, MutationOperator (..))

theOperator :: MutationOperator
theOperator =
  MutationOperator
    { operatorName = "RemoveAction",
      operatorDescription = "Remove one non-binding action from a do block",
      operatorMatch = \case
        le@(L ann (HsDo x ctx (L lann stmts)))
          | any isRemovableStmt stmts ->
              Just (action ann x ctx lann stmts (lhsExprType le))
        _ -> Nothing
    }

-- | A statement is removable if it is a BodyStmt (no binding, not the final
-- return) and not a LastStmt. BindStmt and LetStmt introduce bindings that
-- may be used downstream, so we leave those alone.
isRemovableStmt :: ExprLStmt GhcTc -> Bool
isRemovableStmt (L _ s) = case s of
  BodyStmt {} -> True
  _ -> False

action ::
  SrcSpanAnnA ->
  XDo GhcTc ->
  HsDoFlavour ->
  SrcSpanAnnL ->
  [ExprLStmt GhcTc] ->
  Type ->
  InstrM [(Type, LHsExpr GhcTc, String, String, T.Text -> T.Text)]
action ann x ctx lann stmts ty =
  let removable = [i | (i, s) <- zip [0 ..] stmts, isRemovableStmt s]
      n = length stmts
      mkMutation i =
        let stmts' = take i stmts ++ drop (i + 1) stmts
         in ( ty,
              L ann (HsDo x ctx (L lann stmts')),
              show n ++ " statements",
              show (n - 1) ++ " statements (removed #" ++ show (i + 1) ++ ")",
              id
            )
   in pure [mkMutation i | i <- removable]
