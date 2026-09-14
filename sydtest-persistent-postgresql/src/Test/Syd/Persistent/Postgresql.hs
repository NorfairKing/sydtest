{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    persistPostgresqlReplicatedSpec,
    persistPostgresqlReplicatedSpecWith,
    ReplicatedPools (..),
    onPrimary,
    onReplica,
    awaitReplica,
    unreplicatedPools,
    ReplicaConfig (..),
    defaultReplicaConfig,
    ReplicatedDB (..),
    persistPostgresqlReplicatedAdminSpec,
    persistPostgresqlReplicatedAdminSpecWith,
    replicatedPoolsSpec,

    -- ** Assembling a replica by hand
    -- $read-replica-pieces
    Standby (..),
    replicationPrimaryConfig,
    postgresqlStandbySetupFunc,
    standbyPoolSetupFunc,
    replicatedPoolsSetupFunc,

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
import Data.Time (NominalDiffTime)
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
-- by the caller.
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
-- An application that offloads reads onto a read replica is an application
-- whose reads can be older than its writes. Give its suite one and find out
-- what it does about that:
--
-- > spec :: Spec
-- > spec = persistPostgresqlReplicatedSpec myMigration $
-- >   it "serves what it was given" $ \pools -> do
-- >     thing <- onPrimary pools $ insert something
-- >     awaitReplica pools
-- >     ...
--
-- As much to write as a suite without one.
--
-- 'replicatedPoolsPrimary' and 'replicatedPoolsReplica' are the pair such an
-- application is configured with, so they are the pair to hand it under test.
-- 'awaitReplica' is the one thing a test usually needs beyond them: most of a
-- test arranges what, in production, was arranged long before the request
-- under test, and a replica has all of that.

-- $read-replica-pieces
-- 'persistPostgresqlReplicatedSpec' is these composed. Reach for them when a
-- suite needs the standby somewhere else in its resource stack.

-- A 'Standby' is a real PostgreSQL hot standby streaming from a 'Temp.DB':
-- @pg_basebackup@ into a directory of its own, started with a replication
-- slot, torn down with the suite. It replays the
-- same write-ahead log a production replica does, in the same order, and
-- refuses writes because postgres refuses them.
--
-- Point the read-only half of an application at it to find out what the
-- application does when its reads are older than its writes.
--
-- Note what is a property of the cluster rather than of a database: how far
-- behind the standby is. Tests that share a server share their standby's lag,
-- so a test cannot pause replay or change the delay for itself alone. What it
-- can do is wait for the standby to catch up ('awaitReplica').

-- | A running hot standby of a 'Temp.DB'.
data Standby = Standby
  { -- | Connection options pointing at the standby. The database, user and
    -- password are the primary's; the host and port are the standby's.
    standbyConnectionOptions :: !Postgres.Options,
    standbyDataDirectory :: !FilePath
  }

-- | How far behind the replica is held.
newtype ReplicaConfig = ReplicaConfig
  { -- | Held this far behind, by @recovery_min_apply_delay@.
    --
    -- Zero lets it follow as fast as it can, which on one machine is fast
    -- enough that whether a read sees a just-committed write comes down to
    -- the machine. A delay makes being behind a fact.
    replicaConfigLag :: NominalDiffTime
  }

-- | Twenty milliseconds: what a replica that is keeping up looks like.
--
-- A larger delay tests less, despite sounding harsher. A replica far enough
-- behind to have replayed nothing answers every read with a clean miss, and a
-- clean miss is the case an application handles. Partway caught up, holding
-- one of two rows a query is choosing between, is what a real replica is at
-- any moment.
defaultReplicaConfig :: ReplicaConfig
defaultReplicaConfig = ReplicaConfig {replicaConfigLag = 0.02}

-- | As postgres spells an interval.
renderReplicaLag :: NominalDiffTime -> String
renderReplicaLag lag = show @Integer (round (lag * 1000)) ++ "ms"

-- | A 'SetupFunc' for a hot standby streaming from the given server.
--
-- The server has to have been started with 'replicationPrimaryConfig', or
-- there is no write-ahead log to stream.
postgresqlStandbySetupFunc :: ReplicaConfig -> Temp.DB -> SetupFunc Standby
postgresqlStandbySetupFunc ReplicaConfig {..} db = SetupFunc $ \takeStandby ->
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
            -- Take the checkpoint now rather than spreading it out. The
            -- default spreads it over the checkpoint interval, which is a wait
            -- proportional to how busy the server is, and this server is busy:
            -- a suite is running against it.
            "--checkpoint=fast",
            -- Never dropped, and a physical slot with nothing attached to it
            -- makes a server keep its write-ahead log forever. Harmless here
            -- only because the server this slot is on is torn down moments
            -- after the standby that holds it: compose this against one that
            -- outlives its standby and the log grows without bound.
            "--create-slot",
            "--slot=" ++ slotName
          ]
        ]

    -- tmp-postgres passes the port and socket directory on the command line
    -- rather than through postgresql.conf, so the base backup carries neither
    -- and the standby would come up on the default port.
    --
    -- A socket directory of its own keeps the two servers apart; the
    -- port only names a file inside it, so the standby keeps the primary's.
    -- postgresql.auto.conf, where --write-recovery-conf put primary_conninfo,
    -- is read after this file, so these cannot collide with it.
    appendFile (dataDir </> "postgresql.conf") $
      unlines $
        concat
          [ [ "port = " ++ show primaryPort,
              "unix_socket_directories = '" ++ socketDir ++ "'",
              "listen_addresses = ''",
              "hot_standby = on"
            ],
            ["recovery_min_apply_delay = '" ++ renderReplicaLag replicaConfigLag ++ "'"]
          ]

    let standby =
          Standby
            { standbyConnectionOptions =
                primaryOptions
                  { Postgres.host = pure socketDir
                  },
              standbyDataDirectory = dataDir
            }
    -- pg_ctl says only that the server did not start and to go and read the
    -- log, and the log is in a temporary directory that is about to be
    -- removed, so read it out while it is still there.
    let startStandby =
          runProcessLoudly "pg_ctl" ["-D", dataDir, "-l", logFile, "-w", "start"]
            `onException` (readFile logFile >>= putStr)
        -- Immediate: nothing here is worth a clean shutdown, and a standby
        -- that is deliberately behind would spend the apply delay on one.
        stopStandby = runProcessLoudly "pg_ctl" ["-D", dataDir, "-m", "immediate", "-w", "stop"]
    bracket_ startStandby stopStandby $ takeStandby standby

-- | 'awaitReplica' for a caller that has the two pools loose rather than in a
-- 'ReplicatedPools'.
--
-- A pool is all an application is given, so it is all its test harness can
-- count on having. Returns at once when the second pool is not a standby: a
-- primary is always caught up with itself.
awaitPoolCaughtUp :: ConnectionPool -> ConnectionPool -> IO ()
awaitPoolCaughtUp primaryPool standbyPool = do
  inRecovery <- runSingleQuery standbyPool "SELECT pg_is_in_recovery()"
  when inRecovery $ do
    target <- runSingleQuery primaryPool "SELECT pg_current_wal_insert_lsn()::text"
    let go :: Int -> IO ()
        go attemptsLeft = do
          caughtUp <-
            runSingleQuery
              standbyPool
              ( Text.concat
                  [ "SELECT coalesce(pg_last_wal_replay_lsn() >= '",
                    target,
                    "'::pg_lsn, false)"
                  ]
              )
          unless caughtUp $
            if attemptsLeft <= 0
              then
                expectationFailure $
                  unwords
                    [ "The standby did not replay up to",
                      Text.unpack target,
                      "within",
                      show (pollAttempts * pollInterval `div` 1_000_000),
                      "seconds."
                    ]
              else do
                threadDelay pollInterval
                go (attemptsLeft - 1)
    go pollAttempts

runSingleQuery :: (PersistField a) => ConnectionPool -> Text -> IO a
runSingleQuery pool query =
  runNoLoggingT $
    runSqlPool
      ( do
          rows <- rawSql query []
          case rows of
            [Single a] -> pure a
            _ -> liftIO $ expectationFailure $ unwords ["Expected exactly one row from", show query]
      )
      pool

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
-- differently depending on how far it got.
awaitDatabase :: Postgres.Options -> IO ()
awaitDatabase options = go pollAttempts
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
              threadDelay pollInterval
              go (attemptsLeft - 1)

-- | How long to wait between asking the standby again.
pollInterval :: Int
pollInterval = 10_000 -- 10ms

-- | How many times to ask before calling it a test failure rather than a wait.
--
-- Long enough that reaching it means the standby is never getting there, so
-- that such a standby fails the test rather than hanging the suite.
pollAttempts :: Int
pollAttempts = 6_000 -- a minute's worth

-- | One database, as an application that offloads reads onto a replica sees
-- it: the pool it writes through, and the pool it reads through.
--
-- This is the pair such an application is configured with, so it is the pair
-- to hand it under test.
data ReplicatedPools = ReplicatedPools
  { replicatedPoolsPrimary :: !ConnectionPool,
    replicatedPoolsReplica :: !ConnectionPool
  }

-- | The same database through both, which is what an application configured
-- without a read replica has.
--
-- Everything here works on these: 'awaitReplica' returns at once, because a
-- primary is always caught up with itself.
unreplicatedPools :: ConnectionPool -> ReplicatedPools
unreplicatedPools pool =
  ReplicatedPools
    { replicatedPoolsPrimary = pool,
      replicatedPoolsReplica = pool
    }

-- | Run a query on the primary, which is where an application writes.
--
-- 'runPostgresqlTest' for a suite that has a replica.
onPrimary :: ReplicatedPools -> SqlPersistM a -> IO a
onPrimary = runPostgresqlTest . replicatedPoolsPrimary

-- | Run a query on the replica, which is where an application reads.
--
-- What comes back is what the application would have seen, lag and all.
onReplica :: ReplicatedPools -> SqlPersistM a -> IO a
onReplica = runPostgresqlTest . replicatedPoolsReplica

-- | Wait until the replica has replayed everything the primary had committed
-- when this was called.
--
-- Only ever waiting, never pausing or hurrying the replica: how far behind it
-- is belongs to the cluster, and the tests sharing that cluster are running at
-- the same time as this one.
awaitReplica :: ReplicatedPools -> IO ()
awaitReplica ReplicatedPools {..} =
  awaitPoolCaughtUp replicatedPoolsPrimary replicatedPoolsReplica

-- | A server to write to and a replica of it to read from, for a whole suite.
--
-- One of each: a replica is a second server, and starting one per test would
-- cost more than the tests do.
data ReplicatedDB = ReplicatedDB
  { replicatedDBTemplate :: !TemplateDB,
    replicatedDBStandby :: !Standby
  }

replicatedDBSetupFunc :: ReplicaConfig -> Migration -> SetupFunc ReplicatedDB
replicatedDBSetupFunc config migration = do
  templateDB <- templateDBSetupFunc replicationPrimaryConfig migration
  standby <- postgresqlStandbySetupFunc config (fst templateDB)
  pure
    ReplicatedDB
      { replicatedDBTemplate = templateDB,
        replicatedDBStandby = standby
      }

-- | Declare a test suite that runs against a database and a read replica of
-- it.
--
-- 'persistPostgresqlSpec', with a replica. Every test gets its own database,
-- a pool to it, and a pool to the same database on the replica:
--
-- > spec :: Spec
-- > spec = persistPostgresqlReplicatedSpec migrateExample $
-- >   it "reads back what it wrote" $ \pools -> do
-- >     i <- onPrimary pools $ insert person
-- >     awaitReplica pools
-- >     mPerson <- onReplica pools $ get i
-- >     mPerson `shouldBe` Just person
persistPostgresqlReplicatedSpec ::
  Migration ->
  TestDef (ReplicatedDB ': outers) ReplicatedPools ->
  TestDef outers a
persistPostgresqlReplicatedSpec = persistPostgresqlReplicatedSpecWith defaultReplicaConfig

-- | 'persistPostgresqlReplicatedSpec' with the replica held somewhere other
-- than 'defaultReplicaConfig' holds it.
persistPostgresqlReplicatedSpecWith ::
  ReplicaConfig ->
  Migration ->
  TestDef (ReplicatedDB ': outers) ReplicatedPools ->
  TestDef outers a
persistPostgresqlReplicatedSpecWith config migration =
  persistPostgresqlReplicatedAdminSpecWith config migration
    . replicatedPoolsSpec

-- | A server and a replica of it for the whole suite, and nothing per test.
--
-- 'persistPostgresqlAdminSpec' with a replica, for a suite that builds its own
-- per-test resources around the pair. Suites that want a database per test and
-- nothing else want 'persistPostgresqlReplicatedSpec' instead.
persistPostgresqlReplicatedAdminSpec ::
  Migration ->
  TestDef (ReplicatedDB ': outers) a ->
  TestDef outers a
persistPostgresqlReplicatedAdminSpec = persistPostgresqlReplicatedAdminSpecWith defaultReplicaConfig

-- | 'persistPostgresqlReplicatedAdminSpec' with the replica held somewhere
-- other than 'defaultReplicaConfig' holds it.
persistPostgresqlReplicatedAdminSpecWith ::
  ReplicaConfig ->
  Migration ->
  TestDef (ReplicatedDB ': outers) a ->
  TestDef outers a
persistPostgresqlReplicatedAdminSpecWith config migration =
  setupAroundAll (replicatedDBSetupFunc config migration)

-- | A database and a replica of it per test, from a 'ReplicatedDB' already in
-- the outer stack.
--
-- For a suite that puts other things around its database. Suites that do not
-- want 'persistPostgresqlReplicatedSpec' instead.
replicatedPoolsSpec ::
  (HContains outers ReplicatedDB) =>
  TestDef outers ReplicatedPools ->
  TestDef outers inner
replicatedPoolsSpec =
  setupAroundWith' $ \replicatedDB _ -> replicatedPoolsSetupFunc replicatedDB

-- | A fresh test database with pools to both ends of the replication.
replicatedPoolsSetupFunc :: ReplicatedDB -> SetupFunc ReplicatedPools
replicatedPoolsSetupFunc ReplicatedDB {..} = do
  (options, primaryPool) <- testDatabaseSetupFunc replicatedDBTemplate
  standbyPool <- standbyPoolSetupFunc replicatedDBStandby options
  pure
    ReplicatedPools
      { replicatedPoolsPrimary = primaryPool,
        replicatedPoolsReplica = standbyPool
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
-- by the caller.
persistPostgresqlAdminSpecWith ::
  Temp.Config ->
  Migration ->
  TestDef (TemplateDB ': outers) a ->
  TestDef outers a
persistPostgresqlAdminSpecWith config migration =
  setupAroundAll (templateDBSetupFunc config migration)

templateDBSetupFunc :: Temp.Config -> Migration -> SetupFunc TemplateDB
templateDBSetupFunc config migration = do
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
