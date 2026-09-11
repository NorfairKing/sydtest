-- | Disable annotations that all disable something, and so must compile.
module LiveDisables
  ( arithDisabled,
    allDisabled,
    innerDisabled,
  )
where

{-# ANN arithDisabled ("DisableMutation: Arith" :: String) #-}
arithDisabled :: Int -> Int
arithDisabled n = n + 1

{-# ANN allDisabled ("DisableMutations" :: String) #-}
allDisabled :: Int -> Int
allDisabled n = n + 1

{-# ANN innerDisabled ("DisableMutationsFor inner" :: String) #-}
innerDisabled :: Int -> Int
innerDisabled n =
  let inner = n + 1
   in inner
