module Example.TypeSigLib (diffOf) where

-- | The difference of two numbers, written with an inline type signature.
--
-- The walker has to descend into the signature to reach @a - b@.  Without
-- that, an annotated expression hides its whole subtree from every operator
-- and this function has no mutation site at all.  The signature node itself
-- is not a second site: it has the same type and value as the expression
-- inside it, so mutating both would record every mutation twice.
diffOf :: Int -> Int -> Int
diffOf a b = a - b :: Int
