-- | A binding-level disable naming something that is not an operator.
module UnknownOperator (addOne) where

{-# ANN addOne ("DisableMutation: BoolLt" :: String) #-}
addOne :: Int -> Int
addOne n = n + 1
