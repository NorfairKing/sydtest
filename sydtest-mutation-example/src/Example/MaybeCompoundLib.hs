module Example.MaybeCompoundLib
  ( wrapPair,
  )
where

-- | Regression for the MaybeOp/ConstNothing bug: the operators replaced a
-- @Maybe@-valued expression with a bare @Nothing :: forall a. Maybe a@ instead
-- of instantiating the element type.  For a /compound/ element type — here a
-- tuple that itself contains a @Maybe@, mirroring the shape that first surfaced
-- the crash — the resulting ill-typed Core miscompiled the instrumented site
-- into a runtime segfault (the existing @Maybe Char@ examples never tripped
-- it).  The MaybeOp mutant on @Just p@ exercises 'mkNothingExpr' at element
-- type @(Int, Maybe Int)@; before the fix, merely instrumenting this module
-- crashes the coverage run.
wrapPair :: (Int, Maybe Int) -> Maybe (Int, Maybe Int)
wrapPair p = Just p
