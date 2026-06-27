{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Sanity-check tests for a sqitch project against a temporary
-- PostgreSQL database.
--
-- Three checks are layered on every sqitch project:
--
--   1. /Per-change round-trip/: for each change in the plan, deploy
--      through it, revert one step, redeploy. The schema after the
--      redeploy must match the schema before the revert.
--
--      Skipped in two situations:
--
--        * /Rework heads/ -- the second occurrence of a change name in
--          the plan, whose deploy target ends in @\@HEAD@. Sqitch's
--          revert of just the rework runs the rework's revert script,
--          which by sqitch convention undoes the /whole/ change rather
--          than only the rework, so the intermediate state isn't
--          post(predecessor) and this check would fail spuriously.
--        * /Grandfathered/ steps -- those at or before
--          'sqitchSettingsGrandfatherTag'. These shipped before this
--          test existed and may have minor revert/deploy inconsistencies
--          (e.g. index names that differ between deploy and
--          revert-then-redeploy) that don't matter on the production
--          databases that already ran them.
--
--      The whole-plan cycle test (3) still exercises both of these
--      classes of step end-to-end, and the schema-equality check in
--      @sydtest-sqitch-postgres-persistent@ asserts the final schema
--      matches the persistent model.
--
--   2. /Per-change idempotence/: for each change, re-execute the
--      deploy script's raw SQL against a database where the change has
--      already been applied. The schema must be unchanged. Skipped for
--      grandfathered steps, for the same reason: the failure mode this
--      check guards against (registry drift on retry) cannot bite
--      databases that already successfully ran these scripts.
--
--   3. /Whole-plan deploy/revert/redeploy cycle/: deploy the entire
--      plan, snapshot the schema, revert everything, redeploy the
--      entire plan, snapshot again. The two snapshots must be equal.
--      This exercises both rework heads and grandfathered steps that
--      (1) skips, and also exercises sqitch's own registry across a
--      full cycle.
--
-- Each check runs against a fresh empty database (its own server,
-- user, and DB), allocated and torn down by the spec combinator. The
-- caller never sees the postgres machinery in its outer-type stack.
--
-- Migrations are deployed into a fresh, randomly-named /non-public/
-- schema (created and torn down by 'randomSchemaSetupFunc', put on the
-- search path by 'useTestSchema') rather than @public@. Deploying into a
-- non-default schema makes a migration that hardcodes a schema name (a
-- guard or verify with @table_schema = \'public\'@, say) fail here,
-- instead of passing unnoticed because the test happened to run in
-- @public@.
module Test.Syd.Sqitch.Postgresql
  ( sqitchPostgresqlSpec,
    runSqitchPerChangeChecks,
    runSqitchWholePlanCycle,
    module Test.Syd.Sqitch.Postgresql.Plan,
    module Test.Syd.Sqitch.Postgresql.Process,
    module Test.Syd.Sqitch.Postgresql.Schema,
  )
where

import Control.Monad (forM_, unless)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString as SB
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Database.Persist.Sql as DB
import qualified Database.PostgreSQL.Simple.Options as Postgres
import Path
import Test.Syd
import Test.Syd.Persistent.Postgresql
  ( emptyPostgresOptionsSetupFunc,
    postgresqlPoolSetupFunc,
  )
import Test.Syd.Sqitch.Postgresql.Plan
import Test.Syd.Sqitch.Postgresql.Process
import Test.Syd.Sqitch.Postgresql.Schema

-- | Top-level spec combinator: declares the per-change and whole-plan
-- cycle checks. Allocates fresh empty postgres databases internally
-- for each check, so the caller's outer-type stack is unchanged.
--
-- Sequence with other 'TestDef' values via 'do' or '>>':
--
-- > spec :: Spec
-- > spec = do
-- >   sqitchPostgresqlSpec mySettings
-- >   describe "my other tests" $ ...
sqitchPostgresqlSpec ::
  SqitchSettings ->
  TestDef outers ()
sqitchPostgresqlSpec settings =
  describe "sqitch sanity checks" $
    setupAround emptyPostgresOptionsSetupFunc $ do
      perChangeIt settings
      wholePlanCycleIt settings

perChangeIt :: SqitchSettings -> TestDef outers Postgres.Options
perChangeIt settings =
  it "round-trips and (unless grandfathered) is idempotent for every change in sqitch.plan" $
    \(opts :: Postgres.Options) ->
      runSqitchPerChangeChecks settings opts

wholePlanCycleIt :: SqitchSettings -> TestDef outers Postgres.Options
wholePlanCycleIt settings =
  it "the whole plan deploys, reverts, and redeploys to the same schema" $
    \(opts :: Postgres.Options) ->
      runSqitchWholePlanCycle settings opts

-- | Run the per-change round-trip and idempotence checks against a
-- fresh empty database described by the given options. Exposed in 'IO'
-- so callers can wrap it in 'expectFailing' for negative tests.
runSqitchPerChangeChecks :: SqitchSettings -> Postgres.Options -> IO ()
runSqitchPerChangeChecks settings opts =
  unSetupFunc (postgresqlPoolSetupFunc opts) $ \pool ->
    -- Deploy into a fresh non-public schema, created and torn down here,
    -- so any migration that hardcodes a schema surfaces.
    unSetupFunc (randomSchemaSetupFunc pool) $ \schema -> do
      let target = sqitchTargetFromOptions schema opts

      planRel <- parseRelFile "sqitch.plan"
      steps <-
        readSqitchPlan
          (sqitchSettingsGrandfatherTag settings)
          (sqitchSettingsProjectDir settings </> planRel)

      iterateSteps settings schema target pool steps

-- | Deploy the entire plan, snapshot the schema, revert everything,
-- redeploy the entire plan, snapshot again, assert the two snapshots
-- are equal. Runs against a fresh empty database.
runSqitchWholePlanCycle :: SqitchSettings -> Postgres.Options -> IO ()
runSqitchWholePlanCycle settings opts =
  unSetupFunc (postgresqlPoolSetupFunc opts) $ \pool ->
    unSetupFunc (randomSchemaSetupFunc pool) $ \schema -> do
      let target = sqitchTargetFromOptions schema opts

      sqitchAt settings target "deploy" ["--verify"]
      schemaFirst <- runNoLoggingT $ DB.runSqlPool (useTestSchema schema >> querySchema) pool

      sqitchRevertAll settings target
      sqitchAt settings target "deploy" ["--verify"]
      schemaSecond <- runNoLoggingT $ DB.runSqlPool (useTestSchema schema >> querySchema) pool

      context "whole-plan deploy/revert/redeploy cycle" $
        compareSchemaSnapshots "first deploy" schemaSecond schemaFirst

-- | Walk the plan one step at a time.
--
-- @prevTargets@ pairs each step with the step before it (or 'Nothing'
-- for the first step), so the per-step revert knows where to land. We
-- do not start each step from a clean DB because that would defeat the
-- test's ability to catch FK/dependency interactions between migrations.
iterateSteps ::
  SqitchSettings ->
  Text ->
  SqitchTarget ->
  DB.ConnectionPool ->
  [PlanStep] ->
  IO ()
iterateSteps settings schema target pool steps =
  forM_ (zip steps prevTargets) $ \(step, mPrev) ->
    context (Text.unpack (stepLabel step)) $ do
      sqitchDeployTo settings target (stepDeployTarget step)
      schemaPostStep <-
        runNoLoggingT $ DB.runSqlPool (useTestSchema schema >> querySchema) pool

      -- Round-trip: see module-level docs for the skip conditions.
      unless (stepIsReworkHead step || stepIsGrandfathered step) $ do
        case mPrev of
          Nothing -> sqitchRevertTo settings target "@ROOT"
          Just prev -> sqitchRevertTo settings target (stepDeployTarget prev)
        sqitchDeployTo settings target (stepDeployTarget step)
        schemaAfterRoundtrip <-
          runNoLoggingT $ DB.runSqlPool (useTestSchema schema >> querySchema) pool
        context "round-trip (revert one step then redeploy)" $
          compareSchemaSnapshots "after redeploy" schemaAfterRoundtrip schemaPostStep

      -- Idempotence: re-run the deploy script's raw SQL bypassing
      -- sqitch (which would short-circuit on "already deployed").
      unless (stepIsGrandfathered step) $ do
        script <- readDeployScript settings (stepScriptName step)
        runNoLoggingT $
          flip DB.runSqlPool pool $
            useTestSchema schema >> DB.rawExecute script []
        schemaAfterRerun <-
          runNoLoggingT $ DB.runSqlPool (useTestSchema schema >> querySchema) pool
        context "idempotence (re-run the deploy script)" $
          compareSchemaSnapshots "after rerun" schemaAfterRerun schemaPostStep
  where
    prevTargets = Nothing : map Just steps

readDeployScript :: SqitchSettings -> Text -> IO Text
readDeployScript settings scriptName = do
  deployDir <- parseRelDir "deploy"
  fileRel <- parseRelFile (Text.unpack scriptName <> ".sql")
  Text.decodeUtf8Lenient
    <$> SB.readFile
      (fromAbsFile (sqitchSettingsProjectDir settings </> deployDir </> fileRel))
