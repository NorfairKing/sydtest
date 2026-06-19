{-# LANGUAGE UnboxedTuples #-}

module Example.TupleSwapLib
  ( orderedPair,
    triple,
    diagonal,
    tagChar,
    unboxedPair,
  )
where

-- | A pair of 'Int's in a specific order.  Both components are 'Int', so
-- 'TupleSwap' produces one mutant that swaps them into @(b, a)@.
orderedPair :: Int -> Int -> (Int, Int)
orderedPair a b = (a, b)

-- | Three 'Int' components: 'TupleSwap' swaps each of the three same-typed
-- pairs, producing three mutants.
triple :: Int -> Int -> Int -> (Int, Int, Int)
triple a b c = (a, b, c)

-- | The same variable in both positions.  Every swap is a no-op on textually
-- identical components, so 'TupleSwap' produces nothing.
diagonal :: Int -> (Int, Int)
diagonal v = (v, v)

-- | A pair with differently-typed components.  No two components share a type,
-- so 'TupleSwap' does not fire.
tagChar :: Int -> Char -> (Int, Char)
tagChar n c = (n, c)

-- | An /unboxed/ tuple with two same-typed components.  'TupleSwap' must not
-- fire: an unboxed tuple's type has kind @TYPE (TupleRep ...)@, not @Type@, so
-- wrapping it in @ifMutation \@ty@ (whose type variable is lifted) would build
-- an ill-kinded wrapper that breaks compilation of the instrumented module.
unboxedPair :: Int -> Int -> (# Int, Int #)
unboxedPair a b = (# a, b #)
