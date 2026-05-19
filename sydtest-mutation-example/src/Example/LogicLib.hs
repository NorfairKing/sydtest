module Example.LogicLib
  ( bothPositive,
    eitherPositive,
  )
where

-- | True iff both arguments are strictly positive.
--
-- Mutation sites: LogicOp on @&&@ produces a @||@ alternative.
bothPositive :: Int -> Int -> Bool
bothPositive a b = a > 0 && b > 0

-- | True iff at least one argument is strictly positive.
--
-- Mutation sites: LogicOp on @||@ produces a @&&@ alternative.
eitherPositive :: Int -> Int -> Bool
eitherPositive a b = a > 0 || b > 0
