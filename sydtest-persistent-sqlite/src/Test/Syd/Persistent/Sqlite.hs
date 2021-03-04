{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Testing with an in-memory sqlite database using persistent-sqlite
--
-- For a fully worked example, see sydtest-yesod/blog-example.
module Test.Syd.Persistent.Sqlite
  ( persistSqliteSpec,
    withConnectionPool,
    connectionPoolSetupFunc,
    connectionPoolSetupFunc',
    runSqlPool,
    runSqliteTest,
  )
where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sql
import Database.Persist.Sqlite
import Test.Syd

-- | Declare a test suite that uses a database connection.
--
-- This sets up the database connection around every test, so state is not preserved accross tests.
persistSqliteSpec :: Migration -> SpecWith ConnectionPool -> SpecWith a
persistSqliteSpec migration = aroundWith $ \func _ -> withConnectionPool migration func

-- | Set up a sqlite connection and migrate it to run the given function.
withConnectionPool :: Migration -> (ConnectionPool -> IO ()) -> IO ()
withConnectionPool = flip $ unSetupFunc connectionPoolSetupFunc'

-- | The 'SetupFunc' version of 'withConnectionPool'.
connectionPoolSetupFunc :: Migration -> SetupFunc () ConnectionPool
connectionPoolSetupFunc = unwrapSetupFunc connectionPoolSetupFunc'

-- | A wrapped version of 'connectionPoolSetupFunc'
connectionPoolSetupFunc' :: SetupFunc Migration ConnectionPool
connectionPoolSetupFunc' = SetupFunc $ \takeConnectionPool migration ->
  runNoLoggingT $
    withSqlitePool ":memory:" 1 $ \pool -> do
      _ <- flip runSqlPool pool $ migrationRunner migration
      liftIO $ takeConnectionPool pool

#if MIN_VERSION_persistent(2,10,2)
migrationRunner :: MonadIO m => Migration -> ReaderT SqlBackend m ()
migrationRunner = void . runMigrationQuiet
#else
migrationRunner :: MonadIO m => Migration -> ReaderT SqlBackend m ()
migrationRunner = runMigration
#endif

-- | A flipped version of 'runSqlPool' to run your tests
runSqliteTest :: ConnectionPool -> SqlPersistT IO a -> IO a
runSqliteTest = flip runSqlPool
