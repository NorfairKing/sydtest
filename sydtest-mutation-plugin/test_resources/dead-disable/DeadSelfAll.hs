-- | A binding-level disable of everything on a binding nothing mutates.
module DeadSelfAll (unchanged) where

{-# ANN unchanged ("DisableMutations" :: String) #-}
unchanged :: Int -> Int
unchanged n = n
