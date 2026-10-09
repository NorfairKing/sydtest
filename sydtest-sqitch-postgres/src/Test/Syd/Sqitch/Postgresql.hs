{-# LANGUAGE OverloadedStrings #-}

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
-- Checks (1) and (2) are declared as one test per change, walking the
-- plan in order against a single database. Check (3) gets a database of
-- its own. Each database is a fresh empty one (its own server, user, and
-- DB), allocated and torn down by the spec combinator. The caller never
-- sees the postgres machinery in its outer-type stack.
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
  describe "sqitch sanity checks" $ do
    steps <- runIO $ readPlanSteps settings
    perChangeSpec settings steps
    setupAround emptyPostgresOptionsSetupFunc $ wholePlanCycleIt settings

-- | An empty database to deploy a sqitch plan into, together with the
-- 'SqitchTarget' naming it.
data SqitchDatabase = SqitchDatabase
  { sqitchDatabasePool :: !DB.ConnectionPool,
    sqitchDatabaseSchema :: !Text,
    sqitchDatabaseTarget :: !SqitchTarget
  }

-- | Allocate a fresh empty postgres server, a connection pool to it, and
-- a randomly-named non-public schema to deploy into.
sqitchDatabaseSetupFunc :: SetupFunc SqitchDatabase
sqitchDatabaseSetupFunc = do
  opts <- emptyPostgresOptionsSetupFunc
  sqitchDatabaseSetupFuncFor opts

-- | Like 'sqitchDatabaseSetupFunc', but against a postgres server that
-- the caller already has options for.
sqitchDatabaseSetupFuncFor :: Postgres.Options -> SetupFunc SqitchDatabase
sqitchDatabaseSetupFuncFor opts = do
  pool <- postgresqlPoolSetupFunc opts
  -- A fresh non-public schema, so any migration that hardcodes a schema
  -- surfaces here instead of passing in the default one.
  schema <- randomSchemaSetupFunc pool
  pure
    SqitchDatabase
      { sqitchDatabasePool = pool,
        sqitchDatabaseSchema = schema,
        sqitchDatabaseTarget = sqitchTargetFromOptions schema opts
      }

-- | Declare the per-change round-trip and idempotence checks as one
-- test per change.
--
-- One test per change rather than one test over the whole plan, because
-- sydtest's per-test timeout is a fixed wall-clock budget: with a single
-- test it has to cover every change's sqitch invocations at once, so it
-- shrinks as the plan grows and a loaded machine can blow it. It also
-- makes the failing change a test name instead of whatever happened to
-- be in flight when the clock ran out.
--
-- The tests share one database and walk the plan in order, each
-- deploying on top of what the previous one left behind. That is
-- deliberate: starting every change from a clean database would hide
-- interactions between migrations. It is also why this group must not be
-- run in parallel or in a randomised order.
perChangeSpec :: SqitchSettings -> [PlanStep] -> TestDef outers ()
perChangeSpec settings steps =
  describe "every change round-trips and (unless grandfathered) is idempotent" $
    setupAroundAll sqitchDatabaseSetupFunc $
      doNotRandomiseExecutionOrder $
        sequential $
          forM_ (stepsWithPredecessors steps) $ \(step, mPrev) ->
            itWithOuter (Text.unpack (stepLabel step)) $ \db ->
              checkStep settings db step mPrev

wholePlanCycleIt :: SqitchSettings -> TestDef outers Postgres.Options
wholePlanCycleIt settings =
  it "the whole plan deploys, reverts, and redeploys to the same schema" $
    runSqitchWholePlanCycle settings

-- | Run the per-change round-trip and idempotence checks against a
-- fresh empty database described by the given options. Exposed in 'IO'
-- so callers can wrap it in 'expectFailing' for negative tests.
runSqitchPerChangeChecks :: SqitchSettings -> Postgres.Options -> IO ()
runSqitchPerChangeChecks settings opts =
  unSetupFunc (sqitchDatabaseSetupFuncFor opts) $ \db -> do
    steps <- readPlanSteps settings
    forM_ (stepsWithPredecessors steps) $ \(step, mPrev) ->
      context (Text.unpack (stepLabel step)) $ checkStep settings db step mPrev

-- | Deploy the entire plan, snapshot the schema, revert everything,
-- redeploy the entire plan, snapshot again, assert the two snapshots
-- are equal. Runs against a fresh empty database.
runSqitchWholePlanCycle :: SqitchSettings -> Postgres.Options -> IO ()
runSqitchWholePlanCycle settings opts =
  unSetupFunc (sqitchDatabaseSetupFuncFor opts) $ \db -> do
    let target = sqitchDatabaseTarget db

    sqitchAt settings target "deploy" ["--verify"]
    schemaFirst <- snapshot db

    sqitchRevertAll settings target
    sqitchAt settings target "deploy" ["--verify"]
    schemaSecond <- snapshot db

    context "whole-plan deploy/revert/redeploy cycle" $
      compareSchemaSnapshots "first deploy" schemaSecond schemaFirst

-- | Pair every step with the step before it (or 'Nothing' for the first
-- step), so the per-step revert knows where to land.
stepsWithPredecessors :: [PlanStep] -> [(PlanStep, Maybe PlanStep)]
stepsWithPredecessors steps = zip steps (Nothing : map Just steps)

-- | Deploy one step and check it round-trips and is idempotent.
--
-- Takes the preceding step rather than a clean database, so the round-trip
-- reverts to exactly the state this step was deployed on top of.
checkStep :: SqitchSettings -> SqitchDatabase -> PlanStep -> Maybe PlanStep -> IO ()
checkStep settings db step mPrev = do
  let target = sqitchDatabaseTarget db

  sqitchDeployTo settings target (stepDeployTarget step)
  schemaPostStep <- snapshot db

  -- Round-trip: see module-level docs for the skip conditions.
  unless (stepIsReworkHead step || stepIsGrandfathered step) $ do
    case mPrev of
      Nothing -> sqitchRevertTo settings target "@ROOT"
      Just prev -> sqitchRevertTo settings target (stepDeployTarget prev)
    sqitchDeployTo settings target (stepDeployTarget step)
    schemaAfterRoundtrip <- snapshot db
    context "round-trip (revert one step then redeploy)" $
      compareSchemaSnapshots "after redeploy" schemaAfterRoundtrip schemaPostStep

  -- Idempotence: re-run the deploy script's raw SQL bypassing
  -- sqitch (which would short-circuit on "already deployed").
  unless (stepIsGrandfathered step) $ do
    script <- readDeployScript settings (stepScriptName step)
    runNoLoggingT $
      flip DB.runSqlPool (sqitchDatabasePool db) $
        useTestSchema (sqitchDatabaseSchema db) >> DB.rawExecute script []
    schemaAfterRerun <- snapshot db
    context "idempotence (re-run the deploy script)" $
      compareSchemaSnapshots "after rerun" schemaAfterRerun schemaPostStep

snapshot :: SqitchDatabase -> IO SchemaSnapshot
snapshot db =
  runNoLoggingT $
    DB.runSqlPool
      (useTestSchema (sqitchDatabaseSchema db) >> querySchema)
      (sqitchDatabasePool db)

readPlanSteps :: SqitchSettings -> IO [PlanStep]
readPlanSteps settings = do
  planRel <- parseRelFile "sqitch.plan"
  readSqitchPlan
    (sqitchSettingsGrandfatherTag settings)
    (sqitchSettingsProjectDir settings </> planRel)

readDeployScript :: SqitchSettings -> Text -> IO Text
readDeployScript settings scriptName = do
  deployDir <- parseRelDir "deploy"
  fileRel <- parseRelFile (Text.unpack scriptName <> ".sql")
  Text.decodeUtf8Lenient
    <$> SB.readFile
      (fromAbsFile (sqitchSettingsProjectDir settings </> deployDir </> fileRel))
