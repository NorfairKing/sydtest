module Example.IgnoreLib
  ( mark,
    markPlain,
    markDollar,
  )
where

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
