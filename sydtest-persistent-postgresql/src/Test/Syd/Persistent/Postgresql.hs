{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Testing with a temporary postgresql database using persistent-postgresql
module Test.Syd.Persistent.Postgresql
  ( persistPostgresqlSpec,
    persistPostgresqlAdminSpec,
    persistPostgresqlDatabaseSpec,
    runPostgresqlTest,
    postgresqlMigrationSucceedsSpec,
    connectionPoolSetupFunc,
    TemplateDB,
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Int
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Postgresql
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Database.PostgreSQL.Simple.Options as Options
import qualified Database.PostgreSQL.Simple.Options as Postgres
import Database.Postgres.Temp as Temp
import System.Environment
import System.Random
import Test.Syd
import Test.Syd.Persistent

adminDBSetupFunc :: SetupFunc Temp.DB
adminDBSetupFunc = SetupFunc $ \takeTempDB -> do
  -- Clear PostgreSQL environment variables that might interfere with tmp-postgres
  unsetEnv "PGHOST"
  unsetEnv "PGPORT"
  unsetEnv "PGDATABASE"
  unsetEnv "PGUSER"
  unsetEnv "PGPASSWORD"
  unsetEnv "PGDATA"
  errOrRes <- Temp.withConfig adminConfig takeTempDB
  case errOrRes of
    Left err -> liftIO $ expectationFailure $ show err
    Right r -> pure r

adminConfig :: Temp.Config
adminConfig =
  Temp.defaultConfig
    { Temp.createDbConfig = Temp.Zlich
    }

dbConnectionOptionsPoolSetupFunc :: Postgres.Options -> SetupFunc ConnectionPool
dbConnectionOptionsPoolSetupFunc options =
  SetupFunc $ \takeConnectionPool -> do
    runNoLoggingT $ do
      -- We use a fixed (small) number of connections to avoid overwhelming
      -- the temporary database server that's being called from multiple
      -- tests.
      -- But not 1 to avoid hiding failures that would have come from using
      -- multiple connections in the same test.
      withPostgresqlPool (Options.toConnectionString options) 3 $ \pool -> do
        liftIO $ takeConnectionPool pool

tempUserSetupFunc :: Temp.DB -> SetupFunc (Text, Text)
tempUserSetupFunc db =
  let createUser =
        withAdminConn db $ \conn -> do
          testuser <- genName "user"
          testpassword <- genName "password"
          _ <-
            executeWithLoudFailures
              conn
              ( "CREATE USER "
                  <> fromString (Text.unpack testuser)
                  <> " WITH PASSWORD ?;"
              )
              (PostgreSQL.Only testpassword)
          pure (testuser, testpassword)
      deleteUser (testuser, _) =
        withAdminConn db $ \conn -> do
          _ <-
            executeWithLoudFailures
              conn
              ( "DROP USER "
                  <> fromString (Text.unpack testuser)
                  <> ";"
              )
              ()
          pure ()
   in SetupFunc $ bracket createUser deleteUser

tempNewDatabaseSetupFunc :: Temp.DB -> Text -> SetupFunc Text
tempNewDatabaseSetupFunc db owner =
  let createDB = do
        testdb <- genName "template_db"
        withAdminConn db $ \conn -> do
          _ <-
            executeWithLoudFailures
              conn
              ( "CREATE DATABASE "
                  <> fromString (Text.unpack testdb)
                  <> " OWNER "
                  <> fromString (Text.unpack owner)
                  <> ";"
              )
              ()
          pure testdb
      deleteDB testdb =
        withAdminConn db $ \conn -> do
          _ <-
            executeWithLoudFailures
              conn
              ( "DROP DATABASE "
                  <> fromString (Text.unpack testdb)
                  <> " WITH (FORCE);"
              )
              ()
          pure ()
   in SetupFunc $ bracket createDB deleteDB

tempCopiedDatabaseSetupFunc ::
  Temp.DB ->
  Text ->
  Text ->
  SetupFunc Text
tempCopiedDatabaseSetupFunc
  db
  testuser
  templatedb =
    let createDB = do
          testdb <- genName "test_db"
          withAdminConn db $ \conn -> do
            _ <-
              executeWithLoudFailures
                conn
                ( "CREATE DATABASE "
                    <> fromString (Text.unpack testdb)
                    <> " OWNER "
                    <> fromString (Text.unpack testuser)
                    <> " TEMPLATE "
                    <> fromString (Text.unpack templatedb)
                    <> ";"
                )
                ()

            pure ()
          pure testdb
        deleteDB testdb =
          withAdminConn db $ \conn -> do
            _ <-
              executeWithLoudFailures
                conn
                ( "DROP DATABASE "
                    <> fromString (Text.unpack testdb)
                    <> " WITH (FORCE);"
                )
                ()
            pure ()
     in SetupFunc $ bracket createDB deleteDB

withAdminConn :: Temp.DB -> (PostgreSQL.Connection -> IO a) -> IO a
withAdminConn db =
  bracket
    (PostgreSQL.connectPostgreSQL (toConnectionString db))
    PostgreSQL.close

migrateTempDBSetupFunc :: Temp.DB -> Text -> Text -> Text -> Migration -> SetupFunc ()
migrateTempDBSetupFunc db testuser testpassword testdb migration =
  SetupFunc $ \takeUnit -> do
    let options =
          (toConnectionOptions db)
            { Postgres.user = pure (Text.unpack testuser),
              Postgres.password = pure (Text.unpack testpassword),
              Postgres.dbname = pure (Text.unpack testdb)
            }
    runNoLoggingT $ do
      withPostgresqlPool (Options.toConnectionString options) 1 $ \pool -> do
        runSqlPool (migrationRunner migration) pool
    takeUnit ()

genName :: String -> IO Text
genName prefix = do
  -- This is put into a query without escaping so it must be
  -- alphanumeric
  randomPiece <- replicateM 8 $ randomRIO ('a', 'z')
  pure $ Text.pack $ prefix <> "_" <> randomPiece

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
persistPostgresqlSpec ::
  Migration ->
  TestDef (TemplateDB ': outers) ConnectionPool ->
  TestDef outers a
persistPostgresqlSpec migration =
  persistPostgresqlAdminSpec migration
    . persistPostgresqlDatabaseSpec

type TemplateDB = (Temp.DB, (Text, Text, Text)) -- (db, (templateuser, templatedb))

persistPostgresqlAdminSpec ::
  Migration ->
  TestDef (TemplateDB ': outers) a ->
  TestDef outers a
persistPostgresqlAdminSpec migration =
  setupAroundAll $ do
    db <- adminDBSetupFunc
    (templateuser, templatepassword) <- tempUserSetupFunc db
    templatedb <- tempNewDatabaseSetupFunc db templateuser
    migrateTempDBSetupFunc db templateuser templatepassword templatedb migration
    pure (db, (templateuser, templatepassword, templatedb))

persistPostgresqlDatabaseSpec :: (HContains outers TemplateDB) => TestDef outers ConnectionPool -> TestDef outers a
persistPostgresqlDatabaseSpec =
  setupAroundWith' $ \templatedb _ ->
    connectionPoolSetupFunc templatedb

-- | A 'SetupFunc' that provides a 'ConnectionPool' to a temporary database
connectionPoolSetupFunc ::
  TemplateDB ->
  SetupFunc ConnectionPool
connectionPoolSetupFunc (db, (testuser, testpassword, templatedb)) = do
  testdb <-
    tempCopiedDatabaseSetupFunc
      db
      testuser
      templatedb
  let options =
        (toConnectionOptions db)
          { Postgres.user = pure (Text.unpack testuser),
            Postgres.password = pure (Text.unpack testpassword),
            Postgres.dbname = pure (Text.unpack testdb)
          }
  dbConnectionOptionsPoolSetupFunc options

-- | A flipped version of 'runSqlPool' to run your tests
runPostgresqlTest :: ConnectionPool -> SqlPersistM a -> IO a
runPostgresqlTest = runPersistentTest

-- | Test that the given migration succeeds, when applied to the current database.
--
-- See 'Test.Syd.Persistent.migrationsSucceedsSpec" for details.
postgresqlMigrationSucceedsSpec :: FilePath -> Migration -> TestDef outers void
postgresqlMigrationSucceedsSpec fp migration =
  persistPostgresqlSpec (pure ()) $
    migrationsSucceedsSpecHelper fp migration

executeWithLoudFailures ::
  (PostgreSQL.ToRow a) =>
  PostgreSQL.Connection ->
  PostgreSQL.Query ->
  a ->
  IO Int64
executeWithLoudFailures conn query args =
  PostgreSQL.execute conn query args
    `catch` ( \e -> do
                putStrLn $
                  unlines
                    [ unwords
                        ["Query failed: " ++ show query],
                      displayException (e :: SomeException)
                    ]
                throwIO e
            )
