{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Sanity-check tests for a sqitch project against a temporary
-- PostgreSQL database.
--
-- Two checks are run per change in the plan:
--
--   1. /Round-trip/: deploy through the change, revert one step,
--      redeploy. The schema after the redeploy must match the schema
--      snapshot taken before the revert.
--
--   2. /Idempotence/: re-execute the deploy script's raw SQL against a
--      database where the change has already been applied. The schema
--      must be unchanged. Skipped for changes at or before
--      'sqitchSettingsGrandfatherTag' (see "Test.Syd.Sqitch.Plan").
--
-- After the loop, every change has been deployed; the database is
-- reverted to leave it clean for downstream tests sharing the same pool.
module Test.Syd.Sqitch
  ( sqitchPostgresqlSpec,
    sqitchPostgresqlSpec',
    runSqitchPostgresqlChecks,
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
import Path (parseRelDir, parseRelFile, toFilePath, (</>))
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
    perMigrationSpec settings
    rest

-- | The per-migration round-trip-and-idempotence test, inlined as a
-- single 'it' so a failure points at the right step via 'context'.
perMigrationSpec ::
  forall outers.
  (HContains outers TemplateDB) =>
  SqitchSettings ->
  TestDef outers DB.ConnectionPool
perMigrationSpec settings =
  itWithAll "round-trips and (unless grandfathered) is idempotent for every change in sqitch.plan" $
    \(outers :: HList outers) (pool :: DB.ConnectionPool) ->
      runSqitchPostgresqlChecks settings (getElem outers) pool

-- | Run the round-trip and idempotence checks against an existing pool
-- whose template DB was set up by 'persistPostgresqlSpec'. Exposed
-- separately from 'sqitchPostgresqlSpec' so callers (and this package's
-- own tests for failure modes) can drive the checks in 'IO' and catch
-- the 'ExpectationFailed' that 'shouldBe' throws.
runSqitchPostgresqlChecks ::
  SqitchSettings ->
  TemplateDB ->
  DB.ConnectionPool ->
  IO ()
runSqitchPostgresqlChecks settings tdb pool = do
  testdb <- runNoLoggingT $
    flip DB.runSqlPool pool $ do
      [DB.Single dbName] <- DB.rawSql "SELECT current_database()::text" []
      pure (dbName :: Text)
  let target = sqitchTargetFromTemplateDB tdb testdb

  -- The pool's template DB may have leftovers from prior runs.
  runNoLoggingT $
    flip DB.runSqlPool pool $ do
      DB.rawExecute "DROP SCHEMA IF EXISTS public CASCADE" []
      DB.rawExecute "CREATE SCHEMA public" []
      DB.rawExecute "DROP SCHEMA IF EXISTS sqitch CASCADE" []

  planRel <- parseRelFile "sqitch.plan"
  steps <-
    readSqitchPlan
      (sqitchSettingsGrandfatherTag settings)
      (sqitchSettingsProjectDir settings </> planRel)

  iterateSteps settings target pool steps

  -- Leave the database clean for downstream tests.
  sqitchRevertAll settings target

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
      case mPrev of
        Nothing -> sqitchRevertTo settings target "@ROOT"
        Just prev -> sqitchRevertTo settings target (stepDeployTarget prev)
      sqitchDeployTo settings target (stepDeployTarget step)
      schemaAfterRoundtrip <-
        runNoLoggingT $ DB.runSqlPool querySchema pool
      context "round-trip (revert one step then redeploy)" $
        schemaAfterRoundtrip `shouldBe` schemaPostStep

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
          schemaAfterRerun `shouldBe` schemaPostStep
  where
    prevTargets = Nothing : map Just steps

readDeployScript :: SqitchSettings -> Text -> IO Text
readDeployScript settings scriptName = do
  deployDir <- parseRelDir "deploy"
  fileRel <- parseRelFile (Text.unpack scriptName <> ".sql")
  Text.decodeUtf8Lenient
    <$> ByteString.readFile
      (toFilePath (sqitchSettingsProjectDir settings </> deployDir </> fileRel))
