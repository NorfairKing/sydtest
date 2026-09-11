-- | A local disable whose target is no local binding of the annotated body.
module DeadTarget (withInner) where

{-# ANN withInner ("DisableMutationsFor gone" :: String) #-}
withInner :: Int -> Int
withInner n =
  let inner = n + 1
   in inner
