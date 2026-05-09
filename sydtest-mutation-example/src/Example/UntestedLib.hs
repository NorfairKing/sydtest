module Example.UntestedLib (addOneUntested) where

-- | Add one to the given integer — same as 'Example.Lib.addOne' but with no
-- tests.  This function exists to demonstrate what a surviving mutation looks
-- like: the mutation report will show @IntLit1To0@ for this module surviving
-- because no test exercises it.
addOneUntested :: Int -> Int
addOneUntested n = n + 1
