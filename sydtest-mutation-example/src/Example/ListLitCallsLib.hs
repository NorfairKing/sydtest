module Example.ListLitCallsLib
  ( report,
    reportPlain,
    reportDollar,
  )
where

-- | Stands in for a log or an error function: something whose argument is a
-- message nobody asserts the wording of.  Listed under 'ListLit''s
-- @skip-calls-to@ in the plugin config, so a list literal anywhere inside its
-- argument yields no mutation.
--
-- It answers with a length rather than the message so that 'ElideCall' cannot
-- apply: were this the identity, eliding the call would be an equivalent
-- mutant, and this module is here to exercise 'ListLit' rather than to be a
-- second test of that.
--
-- 'ConstEmptyList' would otherwise fire on the argument lists below, which
-- are the same lists this is about; disable that one operator so what is left
-- is only what 'ListLit' does.
{-# ANN report ("DisableMutation: ConstEmptyList" :: String) #-}
report :: String -> Int
report = length

-- | Direct application: @report (mconcat [...])@.  The list literal is two
-- applications in, since @mconcat@ is the call it is the immediate argument
-- of, so only matching an enclosing call rather than the immediate one
-- suppresses it.
{-# ANN reportPlain ("DisableMutation: ConstEmptyList" :: String) #-}
reportPlain :: String -> Int
reportPlain x = report (mconcat ["problem: ", x])

-- | @$@ application: @report $ mconcat [...]@.  GHC expands @$@ into an
-- @HsApp@ chain whose head is @$@ rather than @report@, so the callee has to
-- be read through it.
{-# ANN reportDollar ("DisableMutation: ConstEmptyList" :: String) #-}
reportDollar :: String -> Int
reportDollar x = report $ mconcat ["problem: ", x]
