module Example.BoolLib
  ( wrapTrue,
    wrapFalse,
    negateWrapped,
  )
where

-- | Wraps True in Just.
--
-- Mutation sites: BoolLit/ConstBool on the inner True, MaybeOp on Just True.
wrapTrue :: Maybe Bool
wrapTrue = Just True

-- | Wraps False in Just.
--
-- Mutation sites: BoolLit/ConstBool on the inner False, MaybeOp on Just False.
wrapFalse :: Maybe Bool
wrapFalse = Just False

-- | Negates a wrapped boolean.
negateWrapped :: Maybe Bool -> Maybe Bool
negateWrapped = fmap not
