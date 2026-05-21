module Example.IgnoreLib
  ( mark,
    markPlain,
    markDollar,
    markM,
    markAction,
  )
where

import Control.Monad.Writer (Writer, execWriter, tell)

-- | A two-argument no-op marker with a polymorphic payload.  Listed
-- under 'ignore:' in the plugin config so mutations are suppressed at
-- every @mark tag payload@ call site, including in the entire argument
-- subtree.  The polymorphic 'a' is what introduces the typecheck-time
-- 'WrapExpr' wrappers around the head that the ignore filter must
-- peel through.
mark :: String -> a -> String
mark tag _ = tag

-- | Direct application: @mark "tag" (unwords [...])@.  The list literal
-- inside the second argument must not be mutated.
markPlain :: String
markPlain = mark "tag" (unwords ["a", "b", "c"])

-- | @$@ application: @mark "tag" $ unwords [...]@.  GHC expands @$@ into
-- an @HsApp@ chain wrapped in 'ExpandedThingTc', and the ignore filter
-- still has to see @mark@ as the head — otherwise mutations leak into
-- the list literal in the second argument.
markDollar :: String
markDollar = mark "tag" $ unwords ["a", "b", "c"]

-- | A no-op marker action, the monadic analogue of 'mark'.  Listed under
-- 'ignore:' so a @do@-block statement @markM "..."@ is left alone.
--
-- This is the @logDebug "..."@ shape: an ignored call used as a
-- non-binding @do@-block statement.  GHC 9.10 expands
-- @do { markM "x"; rest }@ into @(>>) (markM "x") rest@, whose head is
-- @(>>)@, not @markM@.  The 'RemoveAction' operator matches the @(>>)@
-- chain and would otherwise drop the @markM "x"@ action — even though
-- @markM@ is ignored.  The ignore machinery must suppress that removal.
markM :: String -> Writer String ()
markM _ = pure ()

-- | A @do@-block that uses 'markM' (ignored) as a non-binding statement.
-- No mutation may be produced for the @markM "..."@ action, including the
-- 'RemoveAction' that would drop the statement entirely.
markAction :: String
markAction = execWriter $ do
  markM "tag"
  tell "body"
