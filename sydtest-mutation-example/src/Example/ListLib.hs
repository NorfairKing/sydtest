module Example.ListLib
  ( pairConcat,
    tripleConcat,
  )
where

-- | Concatenate a two-element list of strings.
--
-- Mutation sites: ListLit produces an empty-list mutant and a drop-first
-- mutant on the literal @[a, b]@.
pairConcat :: String -> String -> String
pairConcat a b = concat [a, b]

-- | Concatenate a three-element list of strings.
--
-- Mutation sites: ListLit produces an empty-list mutant, a drop-first mutant,
-- and a drop-last mutant on the literal @[a, b, c]@.
tripleConcat :: String -> String -> String -> String
tripleConcat a b c = concat [a, b, c]
