module Example.RemoveClauseLib (Size (..), describeSize) where

-- | A bespoke enum so the only mutation sites in 'describeSize' are
-- RemoveClause removing one of the three equations — there are no literal,
-- list, boolean, or arithmetic sub-expressions for any other operator to
-- touch.
data Size = Empty | Single | Many
  deriving (Eq, Show)

-- | Describe a list by its size.
--
-- Mutation sites: RemoveClause removes one of the three function clauses.
-- Removing the @[]@ or @[_]@ clause makes the input fall through to a later
-- clause (a different 'Size'); removing the @_@ clause makes the function
-- non-exhaustive for longer lists.  Each is killed by the matching test.
describeSize :: [a] -> Size
describeSize [] = Empty
describeSize [_] = Single
describeSize _ = Many
