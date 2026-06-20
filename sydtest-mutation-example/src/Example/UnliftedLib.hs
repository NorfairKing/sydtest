{-# LANGUAGE MagicHash #-}

module Example.UnliftedLib
  ( subInts,
  )
where

import GHC.Exts (Int (I#), Int#, (-#))

-- | Regression for the unlifted-type guard.  @ifMutation :: forall (a ::
-- Type). ...@ can only wrap expressions of kind 'Type'; an operator that wraps
-- an unlifted @Int#@-typed expression builds ill-kinded @ifMutation \@Int#@
-- Core.  The prefix application @minus a b@ below has two same-typed @Int#@
-- arguments, so 'SwitchFunctionArguments' fires on it — exactly the shape that
-- first surfaced this in Alex-generated lexers.  Before the guard, merely
-- instrumenting this module fails.  After it, the @Int#@ sites are skipped (so
-- the module produces no mutations) while it still compiles cleanly.
minus :: Int# -> Int# -> Int#
minus a b = a -# b

subInts :: Int -> Int -> Int
subInts (I# a) (I# b) = I# (minus a b)
