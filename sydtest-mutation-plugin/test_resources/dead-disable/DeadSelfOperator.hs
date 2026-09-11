-- | A binding-level disable naming an operator that mutates nothing in it.
module DeadSelfOperator (addOne) where

-- Arith and IntLit fire on this body; BoolLit has nothing to fire on.
{-# ANN addOne ("DisableMutation: BoolLit" :: String) #-}
addOne :: Int -> Int
addOne n = n + 1
