{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Sanity-check tests for a sqitch project against a temporary
-- PostgreSQL database.
--
-- Three checks are layered on every sqitch project:
--
--   1. /Per-change round-trip/: for each change in the plan, deploy
--      through it, revert one step, redeploy. The schema after the
--      redeploy must match the schema before the revert.
--
--      Skipped for /rework heads/ — the second occurrence of a change
--      name in the plan, whose deploy target ends in @\@HEAD@. Sqitch's
--      revert of just the rework runs the rework's revert script, which
--      by sqitch convention undoes the /whole/ change rather than only
--      the rework, so the intermediate state isn't post(predecessor)
--      and this check would fail spuriously. The whole-plan cycle test
--      (3) still exercises these steps.
--
--   2. /Per-change idempotence/: for each change, re-execute the
--      deploy script's raw SQL against a database where the change has
--      already been applied. The schema must be unchanged. Skipped for
--      changes at or before 'sqitchSettingsGrandfatherTag' (see
--      "Test.Syd.Sqitch.Plan").
--
--   3. /Whole-plan deploy/revert/redeploy cycle/: deploy the entire
--      plan, snapshot the schema, revert everything, redeploy the
--      entire plan, snapshot again. The two snapshots must be equal.
--      This exercises the rework heads that (1) skips, and also
--      exercises sqitch's own registry across a full cycle.
--
-- After every check the database is left clean (everything reverted)
-- for any downstream tests that share the same pool.
module Test.Syd.Sqitch
  ( sqitchPostgresqlSpec,
    sqitchPostgresqlSpec',
    runSqitchPostgresqlChecks,
    runSqitchPerChangeChecks,
    runSqitchWholePlanCycle,
    module Test.Syd.Sqitch.Plan,
    module Test.Syd.Sqitch.Process,
    module Test.Syd.Sqitch.Schema,
  )
where

import Control.Monad (forM_, unless)
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Database.Persist.Sql as DB
import Path
import Test.Syd
import Test.Syd.Persistent.Postgresql
  ( TemplateDB,
    persistPostgresqlSpec,
  )
import Test.Syd.Sqitch.Plan
import Test.Syd.Sqitch.Process
import Test.Syd.Sqitch.Schema

-- | Top-level entry point: sets up a temporary postgres database via
-- 'persistPostgresqlSpec' with an empty migration, then runs the sqitch
-- sanity checks against it.
--
-- The inner test definition shares the same 'TemplateDB' outer setup,
-- so callers can layer extra tests (e.g. the persistent-vs-sqitch
-- schema-equality check provided by @sydtest-sqitch-persistent@).
sqitchPostgresqlSpec ::
  SqitchSettings ->
  TestDef (TemplateDB ': outers) DB.ConnectionPool ->
  TestDef outers a
sqitchPostgresqlSpec settings inner =
  persistPostgresqlSpec (pure ()) $
    sqitchPostgresqlSpec' settings inner

-- | Variant that assumes 'TemplateDB' is already in the outer setup.
sqitchPostgresqlSpec' ::
  (HContains outers TemplateDB) =>
  SqitchSettings ->
  TestDef outers DB.ConnectionPool ->
  TestDef outers DB.ConnectionPool
sqitchPostgresqlSpec' settings rest =
  describe "sqitch sanity checks" $ do
    perChangeSpec settings
    wholePlanCycleSpec settings
    rest

perChangeSpec ::
  forall outers.
  (HContains outers TemplateDB) =>
  SqitchSettings ->
  TestDef outers DB.ConnectionPool
perChangeSpec settings =
  itWithAll "round-trips and (unless grandfathered) is idempotent for every change in sqitch.plan" $
    \(outers :: HList outers) (pool :: DB.ConnectionPool) ->
      runSqitchPerChangeChecks settings (getElem outers) pool

wholePlanCycleSpec ::
  forall outers.
  (HContains outers TemplateDB) =>
  SqitchSettings ->
  TestDef outers DB.ConnectionPool
wholePlanCycleSpec settings =
  itWithAll "the whole plan deploys, reverts, and redeploys to the same schema" $
    \(outers :: HList outers) (pool :: DB.ConnectionPool) ->
      runSqitchWholePlanCycle settings (getElem outers) pool

-- | Backwards-compatible alias for 'runSqitchPerChangeChecks', kept so
-- the negative-case tests of this package don't have to rename.
runSqitchPostgresqlChecks ::
  SqitchSettings ->
  TemplateDB ->
  DB.ConnectionPool ->
  IO ()
runSqitchPostgresqlChecks = runSqitchPerChangeChecks

-- | Run the per-change round-trip and idempotence checks against an
-- existing pool whose template DB was set up by 'persistPostgresqlSpec'.
-- Exposed in 'IO' so callers can wrap it in 'try' for negative tests.
runSqitchPerChangeChecks ::
  SqitchSettings ->
  TemplateDB ->
  DB.ConnectionPool ->
  IO ()
runSqitchPerChangeChecks settings tdb pool = do
  target <- buildTarget tdb pool
  cleanDatabase pool

  planRel <- parseRelFile "sqitch.plan"
  steps <-
    readSqitchPlan
      (sqitchSettingsGrandfatherTag settings)
      (sqitchSettingsProjectDir settings </> planRel)

  iterateSteps settings target pool steps

  -- Leave the database clean for downstream tests.
  sqitchRevertAll settings target

-- | Deploy the entire plan, snapshot the schema, revert everything,
-- redeploy the entire plan, snapshot again, assert the two snapshots
-- are equal. Then revert to leave the database clean.
runSqitchWholePlanCycle ::
  SqitchSettings ->
  TemplateDB ->
  DB.ConnectionPool ->
  IO ()
runSqitchWholePlanCycle settings tdb pool = do
  target <- buildTarget tdb pool
  cleanDatabase pool

  sqitchAt settings target "deploy" ["--verify"]
  schemaFirst <- runNoLoggingT $ DB.runSqlPool querySchema pool

  sqitchRevertAll settings target
  sqitchAt settings target "deploy" ["--verify"]
  schemaSecond <- runNoLoggingT $ DB.runSqlPool querySchema pool

  context "whole-plan deploy/revert/redeploy cycle" $
    compareSchemaSnapshots "first deploy" schemaSecond schemaFirst

  sqitchRevertAll settings target

-- | Build a sqitch target URI for the database the pool is currently
-- connected to.
buildTarget :: TemplateDB -> DB.ConnectionPool -> IO SqitchTarget
buildTarget tdb pool = do
  testdb <- runNoLoggingT $
    flip DB.runSqlPool pool $ do
      [DB.Single dbName] <- DB.rawSql "SELECT current_database()::text" []
      pure (dbName :: Text)
  pure $ sqitchTargetFromTemplateDB tdb testdb

-- | Drop the public schema and sqitch's registry schema, then recreate
-- public. Used at the start of every check so we don't inherit state
-- left behind by an earlier test (or the template DB).
cleanDatabase :: DB.ConnectionPool -> IO ()
cleanDatabase pool =
  runNoLoggingT $
    flip DB.runSqlPool pool $ do
      DB.rawExecute "DROP SCHEMA IF EXISTS public CASCADE" []
      DB.rawExecute "CREATE SCHEMA public" []
      DB.rawExecute "DROP SCHEMA IF EXISTS sqitch CASCADE" []

-- | Walk the plan one step at a time.
--
-- @prevTargets@ pairs each step with the step before it (or 'Nothing'
-- for the first step), so the per-step revert knows where to land. We
-- do not start each step from a clean DB because that would defeat the
-- test's ability to catch FK/dependency interactions between migrations.
iterateSteps ::
  SqitchSettings ->
  SqitchTarget ->
  DB.ConnectionPool ->
  [PlanStep] ->
  IO ()
iterateSteps settings target pool steps =
  forM_ (zip steps prevTargets) $ \(step, mPrev) ->
    context (Text.unpack (stepLabel step)) $ do
      sqitchDeployTo settings target (stepDeployTarget step)
      schemaPostStep <-
        runNoLoggingT $ DB.runSqlPool querySchema pool

      -- Round-trip: revert one step then redeploy. Schema must be
      -- unchanged.
      --
      -- Skipped for rework heads (steps whose deploy target is
      -- @name\@HEAD@). Sqitch's revert of just the rework runs the
      -- rework's revert script, which by convention undoes the whole
      -- change rather than only the rework, so the intermediate state
      -- is not post(predecessor) and this check would fail spuriously.
      -- The whole-plan deploy/revert/redeploy cycle test still covers
      -- these steps.
      unless (stepIsReworkHead step) $ do
        case mPrev of
          Nothing -> sqitchRevertTo settings target "@ROOT"
          Just prev -> sqitchRevertTo settings target (stepDeployTarget prev)
        sqitchDeployTo settings target (stepDeployTarget step)
        schemaAfterRoundtrip <-
          runNoLoggingT $ DB.runSqlPool querySchema pool
        context "round-trip (revert one step then redeploy)" $
          compareSchemaSnapshots "after redeploy" schemaAfterRoundtrip schemaPostStep

      -- Idempotence: re-run the deploy script's raw SQL bypassing
      -- sqitch (which would short-circuit on "already deployed").
      unless (stepIsGrandfathered step) $ do
        script <- readDeployScript settings (stepScriptName step)
        runNoLoggingT $
          flip DB.runSqlPool pool $
            DB.rawExecute script []
        schemaAfterRerun <-
          runNoLoggingT $ DB.runSqlPool querySchema pool
        context "idempotence (re-run the deploy script)" $
          compareSchemaSnapshots "after rerun" schemaAfterRerun schemaPostStep
  where
    prevTargets = Nothing : map Just steps

readDeployScript :: SqitchSettings -> Text -> IO Text
readDeployScript settings scriptName = do
  deployDir <- parseRelDir "deploy"
  fileRel <- parseRelFile (Text.unpack scriptName <> ".sql")
  Text.decodeUtf8Lenient
    <$> ByteString.readFile
      (fromAbsFile (sqitchSettingsProjectDir settings </> deployDir </> fileRel))
