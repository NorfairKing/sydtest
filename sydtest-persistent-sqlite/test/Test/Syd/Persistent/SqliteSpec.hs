module Test.Syd.Persistent.SqliteSpec (spec) where

import Database.Persist
import Database.Persist.Sql
import Test.Syd
import Test.Syd.Persistent.Example
import Test.Syd.Persistent.Sqlite

spec :: Spec
spec =
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
