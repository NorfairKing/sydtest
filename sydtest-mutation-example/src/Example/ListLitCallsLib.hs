module Example.ListLitCallsLib
  ( report,
    Codes (..),
    reportCodes,
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
-- are the same lists this is about. It is answered by the same
-- @skip-calls-to@ entry, under its own name, so there is no pragma here.
report :: String -> Int
report = length

-- | Direct application: @report (mconcat [...])@.  The list literal is two
-- applications in, since @mconcat@ is the call it is the immediate argument
-- of, so only matching an enclosing call rather than the immediate one
-- suppresses it.
reportPlain :: String -> Int
reportPlain x = report (mconcat ["problem: ", x])

-- | @$@ application: @report $ mconcat [...]@.  GHC expands @$@ into an
-- @HsApp@ chain whose head is @$@ rather than @report@, so the callee has to
-- be read through it.
reportDollar :: String -> Int
reportDollar x = report $ mconcat ["problem: ", x]

-- | What 'reportCodes' answers with, so that there is a constructor to name.
newtype Codes = Codes [Int]

-- | A constructor application says what a list is for exactly as a function
-- call does, so 'Codes' is listed under both operators' @skip-calls-to@ in the
-- plugin config and the list literal here yields no mutation.
--
-- A constructor of this module's own rather than one of 'Either''s, so that
-- what is skipped is one named constructor and not every 'Left' in the suite.
reportCodes :: Int -> Codes
reportCodes x = Codes (mconcat [[x], [x]])
