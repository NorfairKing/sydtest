-- | An annotation string that belongs to something else entirely. String
-- annotations are a shared mechanism (dekking's coverage marks functions with
-- "nocover"), so the plugin has to leave one alone rather than complain.
module UnrelatedAnnotation (addOne) where

{-# ANN addOne ("nocover" :: String) #-}
addOne :: Int -> Int
addOne n = n + 1
