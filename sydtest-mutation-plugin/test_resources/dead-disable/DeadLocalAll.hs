-- | A local disable whose target exists but which would be mutated nowhere,
-- so disabling every operator inside it disables nothing.
module DeadLocalAll (withInner) where

{-# ANN withInner ("DisableMutationsFor inner" :: String) #-}
withInner :: Int -> Int
withInner n =
  let inner = n
   in inner
