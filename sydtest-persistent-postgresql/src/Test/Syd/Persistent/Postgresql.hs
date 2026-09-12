{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Testing with a temporary postgresql database using persistent-postgresql
module Test.Syd.Persistent.Postgresql
  ( -- * Spec combinators
    persistPostgresqlSpec,
    persistPostgresqlAdminSpec,
    persistPostgresqlAdminSpecWith,
    persistPostgresqlDatabaseSpec,
    postgresqlMigrationSucceedsSpec,
    runPostgresqlTest,

    -- * Outer-stack handle
    TemplateDB,
    connectionPoolSetupFunc,
    testDatabaseSetupFunc,

    -- * Read replicas
    -- $read-replicas
    Standby (..),
    StandbyConfig (..),
    defaultStandbyConfig,
    replicationPrimaryConfig,
    postgresqlStandbySetupFunc,
    standbyPoolSetupFunc,
    ReplicatedPools (..),
    replicatedPoolsSetupFunc,
    awaitStandbyCaughtUp,

    -- * Lower-level pieces
    -- $building-blocks
    postgresqlServerSetupFunc,
    postgresqlServerSetupFuncWith,
    postgresqlUserSetupFunc,
    postgresqlDatabaseSetupFunc,
    postgresqlPoolSetupFunc,
    emptyPostgresOptionsSetupFunc,
    emptyPostgresPoolSetupFunc,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Int
import Data.Maybe
import Data.Monoid (Last (..))
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Postgresql
import qualified Database.PostgreSQL.Simple as PostgreSQL
import qualified Database.PostgreSQL.Simple.Options as Options
import qualified Database.PostgreSQL.Simple.Options as Postgres
import Database.Postgres.Temp as Temp
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed
import System.Random
import Test.Syd
import Test.Syd.Persistent

-- $building-blocks
-- 'postgresqlServerSetupFunc', 'postgresqlUserSetupFunc', and
-- 'postgresqlDatabaseSetupFunc' are the building blocks that
-- 'persistPostgresqlAdminSpec' and friends are built from. They are
-- exported so other testing libraries can compose their own setup
-- chains — for example, a sanity-check suite that wants a fresh empty
-- database per check without going through a migrated template.

-- | A 'SetupFunc' that spins up a temporary PostgreSQL server via
-- @tmp-postgres@ and tears it down on cleanup. Equivalent to the
-- internal admin/superuser handle the other helpers need.
postgresqlServerSetupFunc :: SetupFunc Temp.DB
postgresqlServerSetupFunc = postgresqlServerSetupFuncWith adminConfig

-- | 'postgresqlServerSetupFunc' with the @tmp-postgres@ configuration chosen
-- by the caller, for a server that has to be more than the default.
--
-- 'Temp.defaultConfig' is tuned for a server nothing else talks to, which is
-- why 'replicationPrimaryConfig' exists: a server that is to have a replica
-- cannot run at @wal_level = minimal@.
postgresqlServerSetupFuncWith :: Temp.Config -> SetupFunc Temp.DB
postgresqlServerSetupFuncWith config = SetupFunc $ \takeTempDB -> do
  -- Clear PostgreSQL environment variables that might interfere with tmp-postgres
  unsetEnv "PGHOST"
  unsetEnv "PGPORT"
  unsetEnv "PGDATABASE"
  unsetEnv "PGUSER"
  unsetEnv "PGPASSWORD"
  unsetEnv "PGDATA"
  errOrRes <- Temp.withConfig config takeTempDB
  case errOrRes of
    Left err -> liftIO $ expectationFailure $ show err
    Right r -> pure r

adminConfig :: Temp.Config
adminConfig =
  Temp.defaultConfig
    { Temp.createDbConfig = Temp.Zlich
    }

-- | 'adminConfig' for a server that can have a standby streaming from it.
--
-- 'Temp.defaultConfig' asks for @wal_level = minimal@, which writes too little
-- to the write-ahead log for anything to replay it, and at which postgres
-- refuses to start with a nonzero @max_wal_senders@. Both have to move
-- together.
replicationPrimaryConfig :: Temp.Config
replicationPrimaryConfig =
  adminConfig
    { Temp.postgresConfigFile =
        Temp.postgresConfigFile adminConfig
          ++ [ ("wal_level", "replica"),
               ("max_wal_senders", "4")
             ]
    }

-- $read-replicas
-- A 'Standby' is a real PostgreSQL hot standby streaming from a 'Temp.DB':
-- @pg_basebackup@ into a directory of its own, started with a replication
-- slot, torn down with the suite. Not a stand-in for one -- it replays the
-- same write-ahead log a production replica does, in the same order, and
-- refuses writes because postgres refuses them, not because the test says so.
--
-- Point the read-only half of an application at it to find out what the
-- application does when its reads are older than its writes.
--
-- Note what is a property of the cluster rather than of a database: how far
-- behind the standby is. Tests that share a server share their standby's lag,
-- so a test cannot pause replay or change the delay for itself alone. What it
-- can do is wait for the standby to catch up ('awaitStandbyCaughtUp').

-- | A running hot standby of a 'Temp.DB'.
data Standby = Standby
  { -- | Connection options pointing at the standby. The database, user and
    -- password are the primary's; the host and port are the standby's.
    standbyConnectionOptions :: !Postgres.Options,
    standbyDataDirectory :: !FilePath
  }

data StandbyConfig = StandbyConfig
  { -- | How far behind the standby is held, as a postgres interval literal
    -- such as @"500ms"@ (@recovery_min_apply_delay@).
    --
    -- 'Nothing' lets it follow as fast as it can, which on a local machine is
    -- fast enough that whether a read sees a just-committed write is a race
    -- rather than a fact. Give it a delay to make being behind the fact.
    standbyConfigMinApplyDelay :: !(Maybe String)
  }

defaultStandbyConfig :: StandbyConfig
defaultStandbyConfig =
  StandbyConfig
    { standbyConfigMinApplyDelay = Nothing
    }

-- | A 'SetupFunc' for a hot standby streaming from the given server.
--
-- The server has to have been started with 'replicationPrimaryConfig', or
-- there is no write-ahead log to stream.
postgresqlStandbySetupFunc :: StandbyConfig -> Temp.DB -> SetupFunc Standby
postgresqlStandbySetupFunc StandbyConfig {..} db = SetupFunc $ \takeStandby ->
  withSystemTempDirectory "sydtest-postgresql-standby" $ \tmpDir -> do
    let dataDir = tmpDir </> "data"
    let socketDir = tmpDir </> "socket"
    let logFile = tmpDir </> "standby.log"
    createDirectoryIfMissing True socketDir

    let primaryOptions = toConnectionOptions db
    let lastOr :: a -> Last a -> a
        lastOr d = fromMaybe d . getLast
    let primaryHost = lastOr "localhost" (Postgres.host primaryOptions)
    let primaryPort = lastOr 5432 (Postgres.port primaryOptions)

    slotName <- Text.unpack <$> genName "standby_slot"
    runProcessLoudly
      "pg_basebackup"
      $ concat
        [ [ "--pgdata=" ++ dataDir,
            "--host=" ++ primaryHost,
            "--port=" ++ show primaryPort
          ],
          -- Only when the server says who to connect as. Left out, libpq falls
          -- back to the operating system user, which is who a server that
          -- names nobody was initialised for.
          ["--username=" ++ user | user <- maybeToList (getLast (Postgres.user primaryOptions))],
          [ -- Writes standby.signal and primary_conninfo, which is what makes
            -- the copy a standby rather than a second copy of the data.
            "--write-recovery-conf",
            "--wal-method=stream",
            "--create-slot",
            "--slot=" ++ slotName
          ]
        ]

    -- Where the standby listens has to be spelled out before it is started.
    -- Not because the base backup got it wrong, but because it never had it:
    -- tmp-postgres passes the port and socket directory to postgres on the
    -- command line rather than through postgresql.conf, so what pg_basebackup
    -- copied says nothing about either and the standby would come up on the
    -- default port.
    --
    -- A socket directory of its own is what keeps the two servers apart. The
    -- port only names a file inside that directory, so the standby can keep
    -- the primary's, which is the one the connection options already carry.
    --
    -- These land in postgresql.conf, and postgresql.auto.conf, where
    -- --write-recovery-conf put primary_conninfo, is read after it, so they
    -- cannot collide with the recovery settings either.
    appendFile (dataDir </> "postgresql.conf") $
      unlines $
        concat
          [ [ "port = " ++ show primaryPort,
              "unix_socket_directories = '" ++ socketDir ++ "'",
              "listen_addresses = ''",
              "hot_standby = on"
            ],
            [ "recovery_min_apply_delay = '" ++ delay ++ "'"
            | delay <- maybeToList standbyConfigMinApplyDelay
            ]
          ]

    let standby =
          Standby
            { standbyConnectionOptions =
                primaryOptions
                  { Postgres.host = pure socketDir
                  },
              standbyDataDirectory = dataDir
            }
    let startStandby = runProcessLoudly "pg_ctl" ["-D", dataDir, "-l", logFile, "-w", "start"]
        -- Immediate: nothing here is worth a clean shutdown, and a standby
        -- that is deliberately behind would spend the apply delay on one.
        stopStandby = runProcessLoudly "pg_ctl" ["-D", dataDir, "-m", "immediate", "-w", "stop"]
    bracket_ startStandby stopStandby $ takeStandby standby

-- | Wait until the standby has replayed everything the primary had committed
-- when this was called.
--
-- For the test that wants the caught-up case, where the rest of the suite has
-- the behind case.
awaitStandbyCaughtUp :: Temp.DB -> Standby -> IO ()
awaitStandbyCaughtUp db standby = do
  target <- withAdminConn db $ \conn -> do
    rows <- PostgreSQL.query_ conn "SELECT pg_current_wal_lsn()::text"
    case rows of
      [PostgreSQL.Only lsn] -> pure (lsn :: Text)
      _ -> throwIO $ userError "BUG: pg_current_wal_lsn returned no row"
  bracket
    (PostgreSQL.connectPostgreSQL (Options.toConnectionString (standbyConnectionOptions standby)))
    PostgreSQL.close
    $ \conn -> do
      let go = do
            rows <-
              PostgreSQL.query
                conn
                "SELECT pg_last_wal_replay_lsn() >= ?::pg_lsn"
                (PostgreSQL.Only target)
            case rows of
              [PostgreSQL.Only True] -> pure ()
              _ -> go
      go

-- | Wait until the database the options name exists on the standby, then open
-- a pool to it.
--
-- The waiting is the point: a database created on the primary is not there
-- until the standby has replayed its creation, and until then a connection
-- fails outright rather than merely finding nothing.
standbyPoolSetupFunc :: Standby -> Postgres.Options -> SetupFunc ConnectionPool
standbyPoolSetupFunc standby options = do
  let standbyOptions =
        options
          { Postgres.host = Postgres.host (standbyConnectionOptions standby),
            Postgres.port = Postgres.port (standbyConnectionOptions standby)
          }
  liftIO $ awaitDatabase standbyOptions
  postgresqlPoolSetupFunc standbyOptions

-- | Retry connecting until it works, or until it has been failing long enough
-- that it is not going to.
--
-- Every failure is worth retrying here: until the standby has replayed the
-- database's creation there is no database to connect to, and libpq says so
-- differently depending on how far it got. The deadline is what turns a
-- standby that is never going to replay this into a test failure that names
-- the database rather than a suite that hangs.
awaitDatabase :: Postgres.Options -> IO ()
awaitDatabase options = go awaitDatabaseAttempts
  where
    go :: Int -> IO ()
    go attemptsLeft = do
      errOrConn <-
        try $
          bracket
            (PostgreSQL.connectPostgreSQL (Options.toConnectionString options))
            PostgreSQL.close
            (\_ -> pure ())
      case errOrConn of
        Right () -> pure ()
        Left (err :: SomeException)
          | attemptsLeft <= 0 ->
              expectationFailure $
                unlines
                  [ unwords
                      [ "The standby never replayed the creation of the database",
                        show (getLast (Postgres.dbname options)) ++ ":"
                      ],
                    displayException err
                  ]
          | otherwise -> do
              threadDelay awaitDatabaseInterval
              go (attemptsLeft - 1)

awaitDatabaseInterval :: Int
awaitDatabaseInterval = 10_000 -- 10ms

awaitDatabaseAttempts :: Int
awaitDatabaseAttempts = 6_000 -- a minute's worth

-- | A fresh test database, as an application that offloads reads onto a
-- replica sees it: a pool to the primary, a pool to the same database on the
-- standby, and the one piece of control over the lag that is safe to hand a
-- single test.
data ReplicatedPools = ReplicatedPools
  { replicatedPoolsPrimary :: !ConnectionPool,
    replicatedPoolsStandby :: !ConnectionPool,
    -- | Wait until the standby has replayed everything the primary has
    -- committed so far, for the test that wants the caught-up case.
    --
    -- Only ever waiting, never pausing or hurrying: how far behind the standby
    -- is belongs to the cluster, and the tests sharing that cluster are running
    -- at the same time as this one.
    replicatedPoolsAwaitCaughtUp :: !(IO ())
  }

-- | A fresh test database with pools to both ends of the replication.
replicatedPoolsSetupFunc ::
  TemplateDB ->
  Standby ->
  SetupFunc ReplicatedPools
replicatedPoolsSetupFunc templateDB standby = do
  (options, primaryPool) <- testDatabaseSetupFunc templateDB
  standbyPool <- standbyPoolSetupFunc standby options
  pure
    ReplicatedPools
      { replicatedPoolsPrimary = primaryPool,
        replicatedPoolsStandby = standbyPool,
        replicatedPoolsAwaitCaughtUp = awaitStandbyCaughtUp (fst templateDB) standby
      }

runProcessLoudly :: String -> [String] -> IO ()
runProcessLoudly cmd args = do
  (ec, out, err) <- readProcess (proc cmd args)
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      expectationFailure $
        unlines
          [ unwords ("Failed to run:" : cmd : args),
            show out,
            show err
          ]

-- | Given libpq-style 'Postgres.Options', allocate a small
-- 'ConnectionPool' to the database those options describe.
postgresqlPoolSetupFunc :: Postgres.Options -> SetupFunc ConnectionPool
postgresqlPoolSetupFunc options =
  SetupFunc $ \takeConnectionPool -> do
    runNoLoggingT $ do
      -- We use a fixed (small) number of connections to avoid overwhelming
      -- the temporary database server that's being called from multiple
      -- tests.
      -- But not 1 to avoid hiding failures that would have come from using
      -- multiple connections in the same test.
      withPostgresqlPool (Options.toConnectionString options) 3 $ \pool -> do
        liftIO $ takeConnectionPool pool

-- | A 'SetupFunc' that creates a fresh PostgreSQL user against the
-- given server (and drops it on cleanup). The result is the
-- @(username, password)@ pair.
postgresqlUserSetupFunc :: Temp.DB -> SetupFunc (Text, Text)
postgresqlUserSetupFunc db =
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

-- | A 'SetupFunc' that creates a fresh empty database owned by the
-- given user against the given server (and drops it on cleanup).
postgresqlDatabaseSetupFunc :: Temp.DB -> Text -> SetupFunc Text
postgresqlDatabaseSetupFunc db owner =
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
persistPostgresqlAdminSpec = persistPostgresqlAdminSpecWith adminConfig

-- | 'persistPostgresqlAdminSpec' with the @tmp-postgres@ configuration chosen
-- by the caller. Pass 'replicationPrimaryConfig' to be able to put a
-- 'postgresqlStandbySetupFunc' behind it.
persistPostgresqlAdminSpecWith ::
  Temp.Config ->
  Migration ->
  TestDef (TemplateDB ': outers) a ->
  TestDef outers a
persistPostgresqlAdminSpecWith config migration =
  setupAroundAll $ do
    db <- postgresqlServerSetupFuncWith config
    (templateuser, templatepassword) <- postgresqlUserSetupFunc db
    templatedb <- postgresqlDatabaseSetupFunc db templateuser
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
connectionPoolSetupFunc templateDB = snd <$> testDatabaseSetupFunc templateDB

-- | 'connectionPoolSetupFunc', but also saying which database the pool is to.
--
-- The name is what a second connection to the same database needs, which is
-- what a standby pool ('standbyPoolSetupFunc') is.
testDatabaseSetupFunc ::
  TemplateDB ->
  SetupFunc (Postgres.Options, ConnectionPool)
testDatabaseSetupFunc (db, (testuser, testpassword, templatedb)) = do
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
  pool <- postgresqlPoolSetupFunc options
  pure (options, pool)

-- | A 'SetupFunc' that provides connection 'Postgres.Options' for a
-- fresh empty database — its own server, its own user, its own empty
-- database, all torn down on cleanup.
--
-- Pair with 'postgresqlPoolSetupFunc' to get a 'ConnectionPool', or
-- use the options directly when an external tool (like @sqitch@)
-- needs them.
--
-- Pay attention to cost: every use of this function spins up a
-- 'Temp.DB' (postgres server start-up is on the order of seconds).
-- For per-test setup, prefer 'setupAroundAll' so the cost is
-- amortised across the suite.
emptyPostgresOptionsSetupFunc :: SetupFunc Postgres.Options
emptyPostgresOptionsSetupFunc = do
  db <- postgresqlServerSetupFunc
  (testuser, testpassword) <- postgresqlUserSetupFunc db
  testdb <- postgresqlDatabaseSetupFunc db testuser
  pure $
    (toConnectionOptions db)
      { Postgres.user = pure (Text.unpack testuser),
        Postgres.password = pure (Text.unpack testpassword),
        Postgres.dbname = pure (Text.unpack testdb)
      }

-- | Convenience: 'emptyPostgresOptionsSetupFunc' threaded through
-- 'postgresqlPoolSetupFunc' to give a 'ConnectionPool' directly.
emptyPostgresPoolSetupFunc :: SetupFunc ConnectionPool
emptyPostgresPoolSetupFunc =
  emptyPostgresOptionsSetupFunc >>= postgresqlPoolSetupFunc

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
