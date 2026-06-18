module Example.ElideCallLib
  ( negateInt,
    absSucc,
    idInt,
    same,
    useSame,
  )
where

-- | 'negate' on an 'Int' returns an 'Int', so 'ElideCall' replaces @negate x@
-- with @x@.  The two differ for any non-zero input, so the test kills it.
negateInt :: Int -> Int
negateInt x = negate x

-- | A nested application.  'ElideCall' fires on both @abs (succ x)@ (replaced
-- with @succ x@) and @succ x@ (replaced with @x@), since each has an 'Int'
-- argument and an 'Int' result.  The test distinguishes all three values.
absSucc :: Int -> Int
absSucc x = abs (succ x)

-- | @id x@ and @x@ are the same value, so an 'ElideCall' mutant here would be
-- equivalent and unkillable.  Calls to 'id' are skipped by default, so
-- 'ElideCall' produces nothing for this function.
idInt :: Int -> Int
idInt x = id x

-- | A user-defined identity wrapper.
same :: Int -> Int
same x = x

-- | Eliding @same x@ to @x@ produces an equivalent, unkillable mutant.  Unlike
-- 'id', user-defined functions are not skipped by default; the in-repo mutation
-- config lists @same@ under the operator's @skip-calls-to@ key, so 'ElideCall'
-- produces nothing for this function.
useSame :: Int -> Int
useSame x = same x
