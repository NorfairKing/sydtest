module Example.RemoveClauseLib (Size (..), describeSize) where

-- | A bespoke enum so that 'describeSize' has no literal, list, boolean, or
-- arithmetic sub-expressions for any operator other than RemoveClause and
-- ConstConstructor to touch.
data Size = Empty | Single | Many
  deriving (Eq, Show)

-- | Describe a list by its size.
--
-- Mutation sites:
--
--   * RemoveClause removes one of the three function clauses.  Removing the
--     @[]@ or @[_]@ clause makes the input fall through to a later clause (a
--     different 'Size'); removing the @_@ clause makes the function
--     non-exhaustive for longer lists.
--   * ConstConstructor replaces each right-hand side with each of the two
--     other 'Size' constructors.
--
-- Each is killed by the matching test.
describeSize :: [a] -> Size
describeSize [] = Empty
describeSize [_] = Single
describeSize _ = Many
