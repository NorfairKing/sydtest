-- | A module-level disable of everything on a module nothing mutates.
module DeadModuleAll (unchanged) where

{-# ANN module ("DisableMutations" :: String) #-}

unchanged :: Int -> Int
unchanged n = n
