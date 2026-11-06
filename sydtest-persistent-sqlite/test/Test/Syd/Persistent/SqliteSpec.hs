module Test.Syd.Persistent.SqliteSpec (spec) where

import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Database.Persist
import Database.Persist.Sql
import Test.Syd
import Test.Syd.OptParse
import Test.Syd.Persistent.Example
import Test.Syd.Persistent.Sqlite

-- | An exception that a test throws itself, but that is classified as
-- asynchronous, as @effectful@'s @Error@ effect does.
-- See https://github.com/NorfairKing/sydtest/issues/110
data SynchronousButClassifiedAsynchronous = SynchronousButClassifiedAsynchronous
  deriving (Show, Eq)

instance Exception SynchronousButClassifiedAsynchronous where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

spec :: Spec
spec = do
  describe "a test that throws an asynchronously-classified exception fails with that exception" $ do
    let settings = defaultSettings {settingRetries = 1}

    it "SqlPersistM ()" $ do
      specForest <-
        execTestDefM settings $
          persistSqliteSpec migrateExample $
            it "throws" (liftIO (throwIO SynchronousButClassifiedAsynchronous) :: SqlPersistM ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> SqlPersistM ()" $ do
      specForest <-
        execTestDefM settings $
          beforeAll (pure ()) $
            setupAround (connectionPoolSetupFunc migrateExample) $
              itWithBoth "throws" (const (liftIO (throwIO SynchronousButClassifiedAsynchronous)) :: () -> SqlPersistM ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

  describe "persistSqliteSpec" $ do
    sqliteMigrationSucceedsSpec "test_resources/migration.sql" migrateExample

    persistSqliteSpec migrateExample $ do
      it "can write and read this example person" $ \pool ->
        runSqliteTest pool $ do
          let p = Person {personName = "John Doe", personAge = Just 21}
          i <- insert p
          mp <- get i
          liftIO $ mp `shouldBe` Just p
      describe "shared data" $ do
        it "can write this example person" $ \pool ->
          runSqliteTest pool $ do
            let p = Person {personName = "John Doe", personAge = Just 21}
            insert_ p
        it "cannot read anything that has not been written yet" $ \pool ->
          runSqliteTest pool $ do
            mp <- get (toSqlKey 0)
            liftIO $ mp `shouldBe` (Nothing :: Maybe Person)
