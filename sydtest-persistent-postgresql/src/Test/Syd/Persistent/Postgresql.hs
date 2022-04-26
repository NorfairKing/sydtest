{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Testing with a temporary postgresql database using persistent-postgresql
module Test.Syd.Persistent.Postgresql
  ( persistPostgresqlSpec,
    withConnectionPool,
    connectionPoolSetupFunc,
    runSqlPool,
    runPostgresqlTest,
  )
where

import Control.Monad.Logger
import Control.Monad.Reader
import Database.Persist.Postgresql
import Database.Postgres.Temp as Temp
import Test.Syd
import Test.Syd.Persistent

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
persistPostgresqlSpec :: Migration -> SpecWith ConnectionPool -> SpecWith a
persistPostgresqlSpec migration = aroundWith $ \func _ -> withConnectionPool migration func

-- | Set up a postgresql connection and migrate it to run the given function.
withConnectionPool :: Migration -> (ConnectionPool -> IO r) -> IO r
withConnectionPool migration func = unSetupFunc (connectionPoolSetupFunc migration) func

-- | The 'SetupFunc' version of 'withConnectionPool'.
connectionPoolSetupFunc :: Migration -> SetupFunc ConnectionPool
connectionPoolSetupFunc migration = SetupFunc $ \takeConnectionPool -> do
  errOrRes <- Temp.with $ \db ->
    runNoLoggingT $
      withPostgresqlPool (toConnectionString db) 1 $ \pool -> do
        _ <- flip runSqlPool pool $ migrationRunner migration
        liftIO $ takeConnectionPool pool
  case errOrRes of
    Left err -> liftIO $ expectationFailure $ show err
    Right r -> pure r

-- | A flipped version of 'runSqlPool' to run your tests
runPostgresqlTest :: ConnectionPool -> SqlPersistM a -> IO a
runPostgresqlTest = runPersistentTest
