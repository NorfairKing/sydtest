{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Testing with an in-memory sqlite database using persistent-sqlite
module Test.Syd.Persistent
  ( runSqlPool,
    runPersistentTest,
    migrationRunner,
    migrationsSucceedsSpecHelper,
  )
where

import Control.Monad
import Control.Monad.Reader
import qualified Data.ByteString as SB
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql
import Test.Syd
import UnliftIO

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

-- | Test that the given migration succeeds, when applied to the current database.
--
-- This uses two tests:
--
-- 1. A golden test for the current migration.
-- 2. A test that first applies the golden migration, and then the current migration, tee see if that fails.
--
-- These tests require ConnectionPools to an _unmigrated_ database.
migrationsSucceedsSpecHelper ::
  -- | Setupfunc for a ConnectionPool. This will be passed an empty migration
  FilePath ->
  Migration ->
  TestDef outers ConnectionPool
migrationsSucceedsSpecHelper migrationFile currentMigration =
  doNotRandomiseExecutionOrder $ do
    descriptionPathHere <- getTestDescriptionPath

    let migrationTestDescription = "Can automatically migrate from the previous database schema"
        migrationTestPath = intercalate "." $ reverse $ migrationTestDescription : map T.unpack descriptionPathHere

        helpTextInMigrationFile =
          [ "ATTENTION CODE REVIEWER",
            "If this file has been updated, please make sure to check",
            "whether this test failed before that happened:",
            show migrationTestPath,
            "If this test failed beforehand, but this golden test has",
            "been updated anyway, that means the current migration is",
            "dangerous with respect to the current database."
          ]

        renderStatements :: [Text] -> Text
        renderStatements ss =
          T.pack $
            unlines $
              concat
                [ map ((<> ";") . T.unpack) ss,
                  [""],
                  map ("-- " <>) helpTextInMigrationFile
                ]
        unrenderStatements :: Text -> [Text]
        unrenderStatements =
          filter (not . T.isPrefixOf "-- ")
            . filter (not . T.null . T.strip)
            . T.lines

    it "Golden test for the current migrations" $ \pool ->
      let helpText =
            unlines
              [ "\nIMPORTANT: Read this message if this test fails.",
                "If this test fails, make check whether the next test has failed as well.",
                "",
                "That test is called ",
                show migrationTestPath,
                "",
                "It passed: All is good, you can reset this golden file safely.",
                "It failed: The database change you introduced will require manual intervention, proceed with caution."
              ]
          gt = goldenTextFile migrationFile (runSqlPool (renderStatements <$> runMigrationQuiet currentMigration) pool)
       in gt
            { goldenTestCompare = \actual expected ->
                let addHelpContext a = Context a helpText
                 in fmap addHelpContext <$> goldenTestCompare gt actual expected
            }

    it migrationTestDescription $ \pool -> do
      textContents <- do
        contents <- SB.readFile migrationFile
        case TE.decodeUtf8' contents of
          Left err -> expectationFailure $ show err
          Right textContents -> pure textContents
      runPersistentTest pool $ do
        let statements = unrenderStatements textContents
        -- Set up the database with the old migrations
        forM_ statements $ \statement ->
          rawExecute statement [] :: SqlPersistM ()
      runPersistentTest pool $ do
        -- Try to run the current migrations
        errOrStatements <-
          (Right <$> runMigrationQuiet currentMigration)
            `catch` (\e -> pure $ Left (e :: PersistException)) ::
            SqlPersistM (Either PersistException [Text])
        case errOrStatements of
          Right _ -> pure ()
          Left err -> liftIO $ case err of
            PersistError t -> expectationFailure $ T.unpack t
            _ -> expectationFailure $ ppShow err
