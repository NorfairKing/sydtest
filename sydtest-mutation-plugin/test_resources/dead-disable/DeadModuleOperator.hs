-- | A module-level disable naming an operator that mutates nothing in the
-- module.
module DeadModuleOperator (addOne) where

{-# ANN module ("DisableMutation: BoolLit" :: String) #-}

addOne :: Int -> Int
addOne n = n + 1
