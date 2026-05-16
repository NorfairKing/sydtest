module Example.LocalDisableLib
  ( withInnerDisabled,
    withInnerBoolLitDisabled,
    withInnerKept,
    withBindDisabled,
    withBindKept,
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

-- | Same idea as 'withInnerDisabled', but the local binding is introduced
-- by a @do@-block @<-@ statement instead of a @let@.  The annotation
-- silences mutations on the RHS of @inner <- toggle b@.
--
-- GHC 9.10+ expands @do@-notation in the renamer, so what the mutation
-- plugin actually sees is @toggle b >>= \\inner -> pure inner@.  The
-- expanded form retains a reference to the original 'BindStmt' through
-- @ExpandedThingTc@, which lets the plugin still apply
-- @DisableMutationsFor@ to the RHS at the call site.
{-# ANN withBindDisabled ("DisableMutationsFor inner" :: String) #-}
withBindDisabled :: (Bool -> IO Bool) -> Bool -> IO Bool
withBindDisabled toggle b = do
  inner <- toggle b
  pure inner

-- | Control for 'withBindDisabled': no disable annotation, so the RHS
-- @toggle b@ is fully instrumented.
withBindKept :: (Bool -> IO Bool) -> Bool -> IO Bool
withBindKept toggle b = do
  inner <- toggle b
  pure inner
