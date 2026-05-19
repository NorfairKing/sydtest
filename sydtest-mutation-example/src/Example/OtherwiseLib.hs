module Example.OtherwiseLib
  ( classify,
  )
where

-- | Classify an integer as negative, zero, or positive.
--
-- Uses 'otherwise' guards, which must not generate no-op mutations
-- (otherwise = True, so @otherwise -> True@ is a semantic no-op).
classify :: Int -> String
classify n
  | n < 0 = "negative"
  | n == 0 = "zero"
  | otherwise = "positive"
