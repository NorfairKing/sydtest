-- | A module-level annotation in a form only a binding accepts: a module has
-- no local bindings to aim a disable at.
module MalformedModuleAnnotation (addOne) where

{-# ANN module ("DisableMutationsFor inner" :: String) #-}

addOne :: Int -> Int
addOne n = n + 1
