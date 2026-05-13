module Example.UntestedLib (addOneUntested) where

-- | Add one to the given integer — same as 'Example.Lib.addOne' but with no
-- tests.  This function exists to demonstrate what an uncovered mutation looks
-- like: the mutation report will show this module's mutations as uncovered
-- because no test exercises it.
{-# ANN addOneUntested ("DisableMutations" :: String) #-}
addOneUntested :: Int -> Int
addOneUntested n = n + 1
