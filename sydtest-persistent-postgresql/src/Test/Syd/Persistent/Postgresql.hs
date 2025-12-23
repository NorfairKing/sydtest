{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Testing with a temporary postgresql database using persistent-postgresql
module Test.Syd.Persistent.Postgresql
  ( persistPostgresqlSpec,
    -- withConnectionPool,
    -- connectionPoolSetupFunc,
    -- runSqlPool,
    runPostgresqlTest,
    postgresqlMigrationSucceedsSpec,
  )
where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Postgresql
import Database.Postgres.Temp as Temp
import Path
import System.Environment (unsetEnv)
import Test.Syd
import Test.Syd.Path
import Test.Syd.Persistent

tempPostgresDBSetupFunc :: SetupFunc Temp.DB
tempPostgresDBSetupFunc =
  tempPostgresDBSetupFuncWith Temp.defaultConfig

tempPostgresDBSetupFuncWith :: Temp.Config -> SetupFunc Temp.DB
tempPostgresDBSetupFuncWith config =
  SetupFunc $ \takeDB -> do
    -- Clear PostgreSQL environment variables that might interfere with tmp-postgres
    liftIO $ do
      unsetEnv "PGHOST"
      unsetEnv "PGPORT"
      unsetEnv "PGDATABASE"
      unsetEnv "PGUSER"
      unsetEnv "PGPASSWORD"
      unsetEnv "PGDATA"
    errOrRes <- Temp.withConfig config $ \db ->
      takeDB db
    case errOrRes of
      Left err -> liftIO $ expectationFailure $ show err
      Right r -> pure r

tempPostgresDBCacheSetupFunc :: SetupFunc Temp.Cache
tempPostgresDBCacheSetupFunc = do
  tmpDir <- tempDirSetupFunc "sydtest-postgresql"
  SetupFunc $
    Temp.withDbCacheConfig
      ( Temp.defaultCacheConfig
          { Temp.cacheTemporaryDirectory = fromAbsDir tmpDir,
            Temp.cacheDirectoryType = Temp.Temporary
          }
      )

tempCachedPostgresDBSetupFunc :: Temp.Cache -> SetupFunc Temp.DB
tempCachedPostgresDBSetupFunc cache =
  tempPostgresDBSetupFuncWith (cacheConfig cache)

tempPostgresSnapshotSetupFunc :: Temp.DB -> SetupFunc Temp.Snapshot
tempPostgresSnapshotSetupFunc db =
  SetupFunc $ \useSnapshot -> do
    errOrRes <- Temp.withSnapshot db $ \snapshot ->
      useSnapshot snapshot
    case errOrRes of
      Left err -> liftIO $ expectationFailure $ show err
      Right r -> pure r

tempSnapshottedPostgresDBSetupFunc :: Temp.Snapshot -> SetupFunc Temp.DB
tempSnapshottedPostgresDBSetupFunc snapshot =
  tempPostgresDBSetupFuncWith (Temp.snapshotConfig snapshot)

snapshottedDBSetupFuncForMigration :: Migration -> SetupFunc Temp.Snapshot
snapshottedDBSetupFuncForMigration migration = do
  liftIO $ putStrLn "Setting up DB cache"
  cache <- tempPostgresDBCacheSetupFunc
  liftIO $ putStrLn "Setting up DB from cache"
  db <- tempCachedPostgresDBSetupFunc cache
  liftIO $ putStrLn "Migrating cached db"

  pool <- tempPostgresDBConnectionPoolSetupFunc db
  migratePoolSetupFunc migration pool

  liftIO $ putStrLn "Taking snapshot of migrated db"
  tempPostgresSnapshotSetupFunc db

tempPostgresDBConnectionPoolSetupFunc :: Temp.DB -> SetupFunc ConnectionPool
tempPostgresDBConnectionPoolSetupFunc db =
  SetupFunc $ \takeConnectionPool -> do
    runNoLoggingT $
      withPostgresqlPool (toConnectionString db) 4 $ \pool -> do
        liftIO $ takeConnectionPool pool

migratePoolSetupFunc :: Migration -> ConnectionPool -> SetupFunc ()
migratePoolSetupFunc migration pool =
  SetupFunc $ \takeUnit -> do
    putStrLn "Running migration"
    flip runSqlPool pool $ migrationRunner migration
    liftIO $ takeUnit ()

type PostgresSpec' outers = TestDef (Temp.Snapshot ': outers) ConnectionPool

type PostgresSpec = PostgresSpec' '[]

-- | Declare a test suite that uses a database connection.
--
-- Example usage
--
-- > -- Database definition
-- > share
-- >   [mkPersist sqlSettings, mkMigrate "migrateExample"]
-- >   [persistLowerCase|
-- > Person
-- >     name String
-- >     age Int Maybe
-- >     deriving Show Eq
-- > |]
-- >
-- > -- Tests
-- > spec :: Spec
-- > spec =
-- >   persistPostgresqlSpec migrateExample $ do
-- >     it "can write and read this example person" $ \pool ->
-- >       runPostgresqlTest pool $ do
-- >         let p = Person {personName = "John Doe", personAge = Just 21}
-- >         i <- insert p
-- >         mp <- get i
-- >         liftIO $ mp `shouldBe` Just p
--
-- This sets up the database connection around every test, so state is not preserved accross tests.
persistPostgresqlSpec :: Migration -> PostgresSpec' outers -> TestDef outers a
persistPostgresqlSpec migration =
  setupAroundAll (snapshottedDBSetupFuncForMigration migration)
    . setupAroundWith'
      ( \snapshot _ -> do
          liftIO $ putStrLn "Creating DB from snapshot"
          db <- tempSnapshottedPostgresDBSetupFunc snapshot
          liftIO $ putStrLn "Created DB from snapshot"
          tempPostgresDBConnectionPoolSetupFunc db
      )

-- -- | Set up a postgresql connection and migrate it to run the given function.
-- withConnectionPool :: Migration -> (ConnectionPool -> IO r) -> IO r
-- withConnectionPool migration func = unSetupFunc (connectionPoolSetupFunc migration) func
--
-- -- | The 'SetupFunc' version of 'withConnectionPool'.
-- connectionPoolSetupFunc :: Migration -> SetupFunc ConnectionPool
-- connectionPoolSetupFunc migration = SetupFunc $ \takeConnectionPool -> do
--   errOrRes <- Temp.with $ \db ->
--     runNoLoggingT $
--       withPostgresqlPool (toConnectionString db) 1 $ \pool -> do
--         _ <- flip runSqlPool pool $ migrationRunner migration
--         liftIO $ takeConnectionPool pool
--   case errOrRes of
--     Left err -> liftIO $ expectationFailure $ show err
--     Right r -> pure r
--

-- | A flipped version of 'runSqlPool' to run your tests
runPostgresqlTest :: ConnectionPool -> SqlPersistM a -> IO a
runPostgresqlTest = runPersistentTest

-- | Test that the given migration succeeds, when applied to the current database.
--
-- See 'Test.Syd.Persistent.migrationsSucceedsSpec" for details.
postgresqlMigrationSucceedsSpec :: FilePath -> Migration -> Spec
postgresqlMigrationSucceedsSpec fp migration =
  setupAroundAll (snapshottedDBSetupFuncForMigration (pure ()))
    . setupAroundWith'
      ( \snapshot _ -> do
          db <- tempSnapshottedPostgresDBSetupFunc snapshot
          tempPostgresDBConnectionPoolSetupFunc db
      )
    $ migrationsSucceedsSpecHelper fp migration
