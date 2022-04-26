{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Testing with an in-memory sqlite database using persistent-sqlite
module Test.Syd.Persistent.Sqlite
  ( persistSqliteSpec,
    withConnectionPool,
    connectionPoolSetupFunc,
    runSqlPool,
    runSqliteTest,
    migrationRunner,
  )
where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Sql
import Database.Persist.Sqlite
import Test.Syd
import Test.Syd.Persistent

-- | Declare a test suite that uses a database connection.
--
-- Example usage
--
--
-- > -- Database Definition
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
-- >   persistSqliteSpec migrateExample $ do
-- >     it "can write and read this example person" $ \pool ->
-- >       runSqliteTest pool $ do
-- >         let p = Person {personName = "John Doe", personAge = Just 21}
-- >         i <- insert p
-- >         mp <- get i
-- >         liftIO $ mp `shouldBe` Just p
--
-- This sets up an in-memory database connection around every test, so state is not preserved accross tests.
persistSqliteSpec :: Migration -> SpecWith ConnectionPool -> SpecWith a
persistSqliteSpec migration = aroundWith $ \func _ -> withConnectionPool migration func

-- | Set up a sqlite connection and migrate it to run the given function.
withConnectionPool :: Migration -> (ConnectionPool -> IO r) -> IO r
withConnectionPool migration func = unSetupFunc (connectionPoolSetupFunc migration) func

-- | The 'SetupFunc' version of 'withConnectionPool'.
connectionPoolSetupFunc :: Migration -> SetupFunc ConnectionPool
connectionPoolSetupFunc migration = SetupFunc $ \takeConnectionPool ->
  runNoLoggingT $
    withSqlitePool ":memory:" 1 $ \pool -> do
      _ <- flip runSqlPool pool $ migrationRunner migration
      liftIO $ takeConnectionPool pool

-- | A flipped version of 'runSqlPool' to run your tests
runSqliteTest :: ConnectionPool -> SqlPersistM a -> IO a
runSqliteTest = runPersistentTest
