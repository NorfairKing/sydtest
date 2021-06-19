{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Testing with an in-memory sqlite database using persistent-sqlite
module Test.Syd.Persistent
  ( runSqlPool,
    runPersistentTest,
    migrationRunner,
  )
where

import Control.Monad.Reader
import Database.Persist.Sql
import Test.Syd

instance IsTest (SqlPersistM ()) where
  type Arg1 (SqlPersistM ()) = ()
  type Arg2 (SqlPersistM ()) = ConnectionPool
  runTest func = runTest (\() -> func)

instance IsTest (outerArgs -> SqlPersistM ()) where
  type Arg1 (outerArgs -> SqlPersistM ()) = outerArgs
  type Arg2 (outerArgs -> SqlPersistM ()) = ConnectionPool
  runTest func = runTest (\outerArgs pool -> runPersistentTest pool (func outerArgs))

-- | A flipped version of 'runSqlPool' to run your tests
runPersistentTest :: ConnectionPool -> SqlPersistM a -> IO a
runPersistentTest = flip runSqlPersistMPool

-- | Helper function to run a 'Migration' before/in a test suite that works accross versions of @persistent@.
#if MIN_VERSION_persistent(2,10,2)
migrationRunner :: MonadIO m => Migration -> ReaderT SqlBackend m ()
migrationRunner = void . runMigrationQuiet
#else
migrationRunner :: MonadIO m => Migration -> ReaderT SqlBackend m ()
migrationRunner = runMigration
#endif
