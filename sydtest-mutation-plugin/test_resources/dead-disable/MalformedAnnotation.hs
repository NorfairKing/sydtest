-- | A binding-level annotation that announces a mutation disable but is none
-- of the recognised forms.
module MalformedAnnotation (addOne) where

{-# ANN addOne ("DisableMutationss: Arith" :: String) #-}
addOne :: Int -> Int
addOne n = n + 1
