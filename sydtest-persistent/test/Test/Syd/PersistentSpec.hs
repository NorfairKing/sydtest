{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.PersistentSpec (spec) where

import Control.Monad.Logger
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Test.Syd
import Test.Syd.Persistent
import Test.Syd.Persistent.Example

spec :: Spec
spec = do
  let persistSpec :: SpecWith ConnectionPool -> Spec
      persistSpec = setupAround $
        SetupFunc $ \takeConnectionPool ->
          runNoLoggingT $
            withSqlitePool ":memory:" 1 $ \pool -> do
              _ <- flip runSqlPool pool $ migrationRunner migrateExample
              liftIO $ takeConnectionPool pool

  persistSpec $ do
    it "can write and read this example person" $ \pool -> runPersistentTest pool $ do
      let p = Person {personName = "John Doe", personAge = Just 21}
      i <- insert p
      mp <- get i
      liftIO $ mp `shouldBe` Just p
    describe "shared data" $ do
      it "can write this example person" $ \pool -> runPersistentTest pool $ do
        let p = Person {personName = "John Doe", personAge = Just 21}
        insert_ p
      it "cannot read anything that has not been written yet" $ \pool -> runPersistentTest pool $ do
        mp <- get (toSqlKey 0)
        liftIO $ mp `shouldBe` (Nothing :: Maybe Person)
