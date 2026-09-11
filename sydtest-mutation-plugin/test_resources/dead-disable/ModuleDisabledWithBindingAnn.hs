-- | A module-level disable of everything, on a module that would be mutated,
-- alongside a binding-level annotation of its own.
--
-- The module-level disable is worth something, so it is not complained about.
-- Neither is the binding-level one, even though the module-level disable has
-- already silenced everything it could disable: the walk that judges the
-- module annotation is thrown away, and a walk that is thrown away reports
-- nothing.
module ModuleDisabledWithBindingAnn (addOne) where

{-# ANN module ("DisableMutations" :: String) #-}

{-# ANN addOne ("DisableMutation: Arith" :: String) #-}
addOne :: Int -> Int
addOne n = n + 1
