module Example.CrossLib
  ( bothPositive,
    eitherPositive,
  )
where

-- | True iff both arguments are strictly positive.
--
-- Mutation sites: LogicOp on @&&@ produces a @||@ alternative.
--
-- This library deliberately ships WITHOUT a test suite of its own.  Its only
-- coverage comes from @sydtest-mutation-example-gen@'s @Example.CrossLibSpec@ —
-- a test suite living in a DIFFERENT package.  The per-library mutation report
-- for this library must attribute that cross-package coverage and kill these
-- mutations rather than reporting them uncovered.  See
-- CROSS_PACKAGE_MUTATION_NOTES.md.
bothPositive :: Int -> Int -> Bool
bothPositive a b = a > 0 && b > 0

-- | True iff at least one argument is strictly positive.
--
-- Mutation sites: LogicOp on @||@ produces a @&&@ alternative.
eitherPositive :: Int -> Int -> Bool
eitherPositive a b = a > 0 || b > 0
