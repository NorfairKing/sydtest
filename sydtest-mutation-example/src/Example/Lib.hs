module Example.Lib (addOne) where

-- | Add one to the given integer.
--
-- The literal '1' here is a mutation site: the plugin will also compile a
-- version where this returns @n + 0@ instead of @n + 1@.
addOne :: Int -> Int
addOne n = n + 1
