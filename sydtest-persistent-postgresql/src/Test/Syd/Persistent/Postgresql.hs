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
import Test.Syd
import Test.Syd.Persistent

tempPostgresDBSetupFunc :: SetupFunc Temp.DB
tempPostgresDBSetupFunc =
  tempPostgresDBSetupFuncWith Temp.defaultConfig

tempPostgresDBSetupFuncWith :: Temp.Config -> SetupFunc Temp.DB
tempPostgresDBSetupFuncWith config =
  SetupFunc $ \takeDB -> do
    errOrRes <- Temp.withConfig config $ \db ->
      takeDB db
    case errOrRes of
      Left err -> liftIO $ expectationFailure $ show err
      Right r -> pure r

tempPostgresDBCacheSetupFunc :: SetupFunc Temp.Cache
tempPostgresDBCacheSetupFunc =
  SetupFunc Temp.withDbCache

tempPostgresDBCacheSetupFuncWith :: Temp.CacheConfig -> SetupFunc Temp.Cache
tempPostgresDBCacheSetupFuncWith config =
  SetupFunc $ Temp.withDbCacheConfig config

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

snapshottedDBSetupFuncForMigration :: Migration -> SetupFunc Temp.DB
snapshottedDBSetupFuncForMigration migration = do
  cache <- tempPostgresDBCacheSetupFunc
  db <- tempCachedPostgresDBSetupFunc cache
  poolBeforeMigration <- tempPostgresDBConnectionPoolSetupFunc db
  _ <- liftIO $ runSqlPool (migrationRunner migration) poolBeforeMigration
  snapshot <- tempPostgresSnapshotSetupFunc db
  tempSnapshottedPostgresDBSetupFunc snapshot

tempPostgresDBConnectionPoolSetupFunc :: Temp.DB -> SetupFunc ConnectionPool
tempPostgresDBConnectionPoolSetupFunc db =
  SetupFunc $ \takeConnectionPool -> do
    runNoLoggingT $
      withPostgresqlPool (toConnectionString db) 4 $ \pool -> do
        liftIO $ takeConnectionPool pool

type PostgresSpec' outers = TestDef (Temp.DB ': outers) ConnectionPool

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
persistPostgresqlSpec :: Migration -> PostgresSpec -> SpecWith a
persistPostgresqlSpec migration =
  setupAroundAll (snapshottedDBSetupFuncForMigration migration)
    . setupAroundWith' (\db _ -> tempPostgresDBConnectionPoolSetupFunc db)

-- | A flipped version of 'runSqlPool' to run your tests
runPostgresqlTest :: ConnectionPool -> SqlPersistM a -> IO a
runPostgresqlTest = runPersistentTest

-- | Test that the given migration succeeds, when applied to the current database.
--
-- See 'Test.Syd.Persistent.migrationsSucceedsSpec" for details.
postgresqlMigrationSucceedsSpec :: FilePath -> Migration -> Spec
postgresqlMigrationSucceedsSpec fp migration =
  setupAroundAll tempPostgresDBCacheSetupFunc
    . setupAroundWith'
      ( \cache _ -> do
          db <- tempCachedPostgresDBSetupFunc cache
          tempPostgresDBConnectionPoolSetupFunc db
      )
    $ migrationsSucceedsSpecHelper fp migration
