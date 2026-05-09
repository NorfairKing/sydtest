-- | Functions in this module are exempt from all mutation instrumentation,
-- demonstrated via a module-level @{-# ANN #-}@ annotation.
module Example.ExceptionLib (addOneModuleDisabled) where

{-# ANN module ("DisableMutations" :: String) #-}

-- | All mutations disabled on this whole module via
-- @{-# ANN module ("DisableMutations" :: String) #-}@.
addOneModuleDisabled :: Int -> Int
addOneModuleDisabled n = n + 1
