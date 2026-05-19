module Example.CaseLib (describeList) where

-- | Describe a list by its size.
--
-- Mutation sites: RemoveCase removes one of the three alternatives.
describeList :: [a] -> String
describeList xs = case xs of
  [] -> "empty"
  [_] -> "singleton"
  _ -> "longer"
