{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Testing with a temporary postgresql database using persistent-postgresql
module Test.Syd.Persistent.Postgresql
  ( persistPostgresqlSpec,
    runPostgresqlTest,
    postgresqlMigrationSucceedsSpec,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Exception
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.ByteString as SB
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Database.Persist.Postgresql
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Database.PostgreSQL.Simple.Options as Options
import qualified Database.PostgreSQL.Simple.Options as Postgres
import Database.Postgres.Temp as Temp
import System.Environment
import System.Random
import Test.Syd
import Test.Syd.Persistent

tempDBSetupFunc :: SetupFunc Temp.DB
tempDBSetupFunc = SetupFunc $ \takeTempDB -> do
  errOrRes <- Temp.withConfig adminConfig $ \db -> do
    -- Clear PostgreSQL environment variables that might interfere with tmp-postgres
    -- TODO upstream this
    unsetEnv "PGHOST"
    unsetEnv "PGPORT"
    unsetEnv "PGDATABASE"
    unsetEnv "PGUSER"
    unsetEnv "PGPASSWORD"
    unsetEnv "PGDATA"

    takeTempDB db
  case errOrRes of
    Left err -> liftIO $ expectationFailure $ show err
    Right r -> pure r

adminConfig :: Temp.Config
adminConfig =
  Temp.defaultConfig
    { Temp.createDbConfig = Temp.Zlich
    }

dbConnectionPoolSetupFunc :: Temp.DB -> SetupFunc ConnectionPool
dbConnectionPoolSetupFunc db =
  SetupFunc $ \takeConnectionPool -> do
    runNoLoggingT $
      -- TODO multiple connections
      withPostgresqlPool (toConnectionString db) 1 $ \pool -> do
        liftIO $ takeConnectionPool pool

dbConnectionOptionsPoolSetupFunc :: Postgres.Options -> SetupFunc ConnectionPool
dbConnectionOptionsPoolSetupFunc options =
  SetupFunc $ \takeConnectionPool -> do
    runNoLoggingT $ do
      connectionCount <- liftIO getNumCapabilities -- Assuming postgres runs on the same machine.
      withPostgresqlPool (Options.toConnectionString options) 1 $ \pool -> do
        liftIO $ takeConnectionPool pool

adminDBSetupFunc :: SetupFunc Temp.DB
adminDBSetupFunc = tempDBSetupFunc

genName :: String -> IO Text
genName prefix = do
  -- This is put into a query without escaping so it must be
  -- alphanumeric
  randomPiece <- replicateM 32 $ randomRIO ('a', 'z')
  pure $ Text.pack $ prefix <> "_" <> randomPiece

-- Create a per-test user and database to avoid test pollution
-- while only using one postgresql server
testDBSetupFunc :: Temp.DB -> SetupFunc ConnectionPool
testDBSetupFunc db = do
  let adminOptions = toConnectionOptions db

  testuser <- liftIO $ genName "user"
  testpassword <- liftIO $ genName "password"
  testdb <- liftIO $ genName "db"

  let withAdminConn =
        bracket
          (PostgreSQL.connectPostgreSQL (toConnectionString db))
          PostgreSQL.close

  let createUserAndDB =
        withAdminConn $ \conn -> do
          PostgreSQL.execute
            conn
            ( "CREATE USER "
                <> fromString (Text.unpack testuser)
                <> " WITH PASSWORD ?;"
            )
            (PostgreSQL.Only testpassword)

          PostgreSQL.execute
            conn
            ( "CREATE DATABASE "
                <> fromString (Text.unpack testdb)
                <> " OWNER "
                <> fromString (Text.unpack testuser)
                <> ";"
            )
            ()
          pure ()

  let cleanupUserAndDB =
        withAdminConn $ \conn -> do
          PostgreSQL.execute
            conn
            ( "DROP DATABASE "
                <> fromString (Text.unpack testdb)
                <> ";"
            )
            ()
          PostgreSQL.execute
            conn
            ( "DROP USER "
                <> fromString (Text.unpack testuser)
                <> ";"
            )
            ()
          pure ()

  SetupFunc $ \takeUnit ->
    bracket_ createUserAndDB cleanupUserAndDB (takeUnit ())

  let options =
        adminOptions
          { Postgres.user = pure (Text.unpack testuser),
            Postgres.password = pure (Text.unpack testpassword),
            Postgres.dbname = pure (Text.unpack testdb)
          }
  dbConnectionOptionsPoolSetupFunc options

migratePoolSetupFunc :: Migration -> ConnectionPool -> SetupFunc ()
migratePoolSetupFunc migration pool =
  liftIO $ runSqlPool (migrationRunner migration) pool

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
persistPostgresqlSpec :: Migration -> TestDef (Temp.DB ': outers) ConnectionPool -> TestDef outers a
persistPostgresqlSpec migration =
  setupAroundAll adminDBSetupFunc
    . setupAroundWith'
      ( \db _ -> do
          pool <- testDBSetupFunc db
          migratePoolSetupFunc migration pool
          pure pool
      )

-- | The 'SetupFunc' version of 'withConnectionPool'.
connectionPoolSetupFunc :: Migration -> SetupFunc ConnectionPool
connectionPoolSetupFunc migration = do
  db <- tempDBSetupFunc
  pool <- dbConnectionPoolSetupFunc db
  liftIO $ runSqlPool (migrationRunner migration) pool
  pure pool

-- | A flipped version of 'runSqlPool' to run your tests
runPostgresqlTest :: ConnectionPool -> SqlPersistM a -> IO a
runPostgresqlTest = runPersistentTest

-- | Test that the given migration succeeds, when applied to the current database.
--
-- See 'Test.Syd.Persistent.migrationsSucceedsSpec" for details.
postgresqlMigrationSucceedsSpec :: FilePath -> Migration -> Spec
postgresqlMigrationSucceedsSpec fp migration =
  persistPostgresqlSpec (pure ()) $
    migrationsSucceedsSpecHelper fp migration
