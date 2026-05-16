module Example.LocalDisableLib
  ( withInnerDisabled,
    withInnerBoolLitDisabled,
    withInnerKept,
  )
where

-- | Top-level function whose local @inner@ binding has all mutations
-- disabled via @DisableMutationsFor inner@.
--
-- @inner@'s RHS @not b@ is the natural target the annotation silences:
-- without it, mutations on the literal-less expression here (such as
-- @ConstBool@ wrapping @b@ in @True/False@, or @Negate@) would survive
-- any spec test that only fixes the externally observable result.
{-# ANN withInnerDisabled ("DisableMutationsFor inner" :: String) #-}
withInnerDisabled :: Bool -> Bool
withInnerDisabled b =
  let inner = not b
   in inner

-- | Like 'withInnerDisabled', but only @ConstBool@ is disabled inside
-- @inner@'s RHS. Other operators on @inner@'s RHS (notably the @Negate@
-- operator on @not b@) still fire.
{-# ANN withInnerBoolLitDisabled ("DisableMutationsFor inner: ConstBool" :: String) #-}
withInnerBoolLitDisabled :: Bool -> Bool
withInnerBoolLitDisabled b =
  let inner = not b
   in inner

-- | Control: no disable annotation. Both @ConstBool@ and @Negate@ fire
-- on the @inner@ binding's RHS.
withInnerKept :: Bool -> Bool
withInnerKept b =
  let inner = not b
   in inner
