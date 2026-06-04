module Example.ConstFnLib
  ( safeHead,
    firstChar,
    keepDigits,
    digitsOf,
    implies,
    impliesPair,
    majorityOf5,
    majorityOf5Wrapper,
  )
where

import Data.Char (isDigit)

-- | The head of a list as 'Maybe'.
--
-- Sites here: the bare @Nothing@ in the empty-list branch is already
-- 'Nothing'; the @Just x@ branch is constructor-headed so 'ConstNothing'
-- skips it ('MaybeOp' handles that one).
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- | Get the first character of a 'String' as 'Maybe'.
--
-- Mutation sites in @firstChar s = safeHead s@:
--
--   * 'ConstNothing' arity 0 on the application @safeHead s :: Maybe Char@:
--     mutated to @firstChar s = Nothing@.
--   * 'ConstNothing' arity 1 on the bare @safeHead :: String -> Maybe Char@:
--     mutated to @firstChar s = (\\_ -> Nothing) s@.
firstChar :: String -> Maybe Char
firstChar s = safeHead s

-- | Keep only the digit characters from a list.
--
-- Helper used by 'digitsOf' to give that call site a non-literal
-- list-returning function to mutate.
keepDigits :: String -> String
keepDigits = filter isDigit

-- | Get the digits of a 'String'.
--
-- Mutation sites in @digitsOf s = keepDigits s@:
--
--   * 'ConstEmptyList' arity 0 on the application @keepDigits s :: String@:
--     mutated to @digitsOf s = []@.
--   * 'ConstEmptyList' arity 1 on the bare @keepDigits :: String -> String@:
--     mutated to @digitsOf s = (\\_ -> []) s@.
digitsOf :: String -> String
digitsOf s = keepDigits s

-- | Logical implication.  Used to exercise 'ConstBool' at arity 2.
implies :: Bool -> Bool -> Bool
implies a b = not a || b

-- | A wrapper around 'implies' whose body references 'implies' at arity 2
-- as a bare (un-applied) name.
--
-- Mutation sites on the bare @implies@ in the body:
--
--   * 'ConstBool' arity 2 produces @(\\_ _ -> True)@ and
--     @(\\_ _ -> False)@ mutants.
impliesPair :: (Bool, Bool) -> Bool
impliesPair (a, b) = implies a b

-- | A 5-argument predicate.  Used to confirm the 'ConstBool' arity-5
-- mutation produces a well-formed prefix-form preview.
majorityOf5 :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool
majorityOf5 a b c d e =
  let n = length (filter id [a, b, c, d, e]) in n >= 3

-- | A wrapper that references 'majorityOf5' as a bare arity-5 name.
--
-- Mutation site on the bare @majorityOf5@:
--
--   * 'ConstBool' arity 5 produces @(\\_ _ _ _ _ -> True)@ and
--     @(\\_ _ _ _ _ -> False)@ mutants.  At a non-infix site this uses
--     the plain 'TokenReplace' delta (no 'ReplaceOuterSpan' is needed).
majorityOf5Wrapper :: (Bool, Bool, Bool, Bool, Bool) -> Bool
majorityOf5Wrapper (a, b, c, d, e) = majorityOf5 a b c d e
