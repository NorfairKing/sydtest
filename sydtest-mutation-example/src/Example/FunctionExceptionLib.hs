-- | Functions in this module demonstrate per-function mutation exceptions
-- via @{-# ANN functionName ("DisableMutation: ..." :: String) #-}@ annotations.
module Example.FunctionExceptionLib
  ( addOneArithDisabled,
    addOneArithAndIntLitDisabled,
    addOneFunctionDisabled,
    constBoolDisabledPolymorphic,
  )
where

-- | Arith mutations disabled on this function only.
-- IntLit mutations (replacing the literal @1@) still apply.
{-# ANN addOneArithDisabled ("DisableMutation: Arith" :: String) #-}
addOneArithDisabled :: Int -> Int
addOneArithDisabled n = n + 1

-- | Both Arith and IntLit mutations disabled on this function.
{-# ANN addOneArithAndIntLitDisabled ("DisableMutations: Arith, IntLit" :: String) #-}
addOneArithAndIntLitDisabled :: Int -> Int
addOneArithAndIntLitDisabled n = n + 1

-- | All mutations disabled on this function via
-- @{-# ANN addOneFunctionDisabled ("DisableMutations" :: String) #-}@.
{-# ANN addOneFunctionDisabled ("DisableMutations" :: String) #-}
addOneFunctionDisabled :: Int -> Int
addOneFunctionDisabled n = n + 1

-- | All mutations disabled on a polymorphic function (which GHC wraps in AbsBinds).
-- Without the AbsBinds fix in the plugin, the ANN on the poly Id is not propagated
-- to the inner mono binding, so mutations would still fire and be uncovered (no tests
-- exist for this function by design), causing the mutation check to fail.
{-# ANN constBoolDisabledPolymorphic ("DisableMutations" :: String) #-}
constBoolDisabledPolymorphic :: (Ord a, Num a) => a -> Bool
constBoolDisabledPolymorphic n = n > 0
