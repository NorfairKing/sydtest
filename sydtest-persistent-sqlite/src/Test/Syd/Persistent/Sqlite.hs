{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Persistent.Sqlite
  ( persistSqliteSpec,
    withConnectionPool,
    runSqlPool,
    runSqliteTest,
  )
where

import Control.Monad.Logger
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
withConnectionPool migration func = do
  runNoLoggingT $
    withSqlitePool ":memory:" 1 $ \pool -> do
      _ <- flip runSqlPool pool $ runMigrationQuiet migration
      liftIO $ func pool

-- | A flipped version of 'runSqlPool' to run your tests
runSqliteTest :: ConnectionPool -> SqlPersistT IO a -> IO a
runSqliteTest = flip runSqlPool
