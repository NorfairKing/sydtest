{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Testing with a temporary postgresql database using persistent-postgresql
module Test.Syd.Persistent.Postgresql
  ( persistPostgresqlSpec,
    withConnectionPool,
    connectionPoolSetupFunc,
    connectionPoolSetupFunc',
    runSqlPool,
    runPostgresqlTest,
  )
where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Postgres.Temp as Temp
import Test.Syd

-- | Declare a test suite that uses a database connection.
--
-- This sets up the database connection around every test, so state is not preserved accross tests.
persistPostgresqlSpec :: Migration -> SpecWith ConnectionPool -> SpecWith a
persistPostgresqlSpec migration = aroundWith $ \func _ -> withConnectionPool migration func

-- | Set up a postgresql connection and migrate it to run the given function.
withConnectionPool :: Migration -> (ConnectionPool -> IO ()) -> IO ()
withConnectionPool = flip $ unSetupFunc connectionPoolSetupFunc'

-- | The 'SetupFunc' version of 'withConnectionPool'.
connectionPoolSetupFunc :: Migration -> SetupFunc () ConnectionPool
connectionPoolSetupFunc = unwrapSetupFunc connectionPoolSetupFunc'

-- | A wrapped version of 'connectionPoolSetupFunc'
connectionPoolSetupFunc' :: SetupFunc Migration ConnectionPool
connectionPoolSetupFunc' = SetupFunc $ \takeConnectionPool migration -> do
  errOrRes <- Temp.with $ \db ->
    runNoLoggingT $
      withPostgresqlPool (toConnectionString db) 1 $ \pool -> do
        _ <- flip runSqlPool pool $ migrationRunner migration
        liftIO $ takeConnectionPool pool
  case errOrRes of
    Left err -> liftIO $ expectationFailure $ show err
    Right r -> pure r

#if MIN_VERSION_persistent(2,10,2)
migrationRunner :: MonadIO m => Migration -> ReaderT SqlBackend m ()
migrationRunner = void . runMigrationQuiet
#else
migrationRunner :: MonadIO m => Migration -> ReaderT SqlBackend m ()
migrationRunner = runMigration
#endif

-- | A flipped version of 'runSqlPool' to run your tests
runPostgresqlTest :: ConnectionPool -> SqlPersistT IO a -> IO a
runPostgresqlTest = flip runSqlPool
