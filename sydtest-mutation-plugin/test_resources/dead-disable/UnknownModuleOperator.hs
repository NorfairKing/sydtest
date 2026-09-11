-- | A module-level disable naming something that is not a mutation operator.
module UnknownModuleOperator (addOne) where

{-# ANN module ("DisableMutation: BoolLt" :: String) #-}

addOne :: Int -> Int
addOne n = n + 1
