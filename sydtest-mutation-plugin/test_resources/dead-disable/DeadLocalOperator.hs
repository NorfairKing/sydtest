-- | A local disable whose target exists but which names an operator that
-- mutates nothing inside it.
module DeadLocalOperator (withInner) where

{-# ANN withInner ("DisableMutationFor inner: BoolLit" :: String) #-}
withInner :: Int -> Int
withInner n =
  let inner = n + 1
   in inner
