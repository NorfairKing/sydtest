{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Persistent.Postgresql.StandbySpec (spec) where

import Database.Persist
import Database.Persist.Sql (rawExecute, runSqlPoolNoTransaction)
import Database.PostgreSQL.Simple (SqlError (..))
import Test.Syd
import Test.Syd.Persistent.Example
import Test.Syd.Persistent.Postgresql

spec :: Spec
spec =
  describe "persistPostgresqlReplicatedSpec" $
    persistPostgresqlReplicatedSpec migrateExample $ do
      it "does not show a write on the replica while it is behind" $ \pools -> do
        let p = Person {personName = "John Doe", personAge = Just 21}
        i <- onPrimary pools $ insert p
        mPerson <- onReplica pools $ get i
        mPerson `shouldBe` Nothing

      it "shows the write on the replica once it has caught up" $ \pools -> do
        let p = Person {personName = "Jane Doe", personAge = Just 22}
        i <- onPrimary pools $ insert p
        awaitReplica pools
        mPerson <- onReplica pools $ get i
        mPerson `shouldBe` Just p

      -- A hot standby is read-only because postgres refuses the write, not
      -- because the test was told to pretend.
      it "refuses a write through the replica" $ \pools -> do
        let p = Person {personName = "Jim Doe", personAge = Just 23}
        onReplica pools (insert_ p)
          `shouldThrow` (\(e :: SqlError) -> sqlState e == "25006")

      -- An application configured without a read replica reads and writes
      -- through the one pool it has, and everything here still works on it.
      it "reads back at once through pools that are not replicated" $ \pools -> do
        let unreplicated = unreplicatedPools (replicatedPoolsPrimary pools)
        let p = Person {personName = "Jess Doe", personAge = Just 25}
        i <- onPrimary unreplicated $ insert p
        awaitReplica unreplicated
        mPerson <- onReplica unreplicated $ get i
        mPerson `shouldBe` Just p

      -- A read on the standby holds a snapshot.  If the primary is free to
      -- remove the row versions that snapshot needs, replaying the cleanup
      -- leaves the standby no choice but to cancel the read, and the
      -- application sees SQLSTATE 40001 rather than its data.
      --
      -- Nothing the application does can avoid that, so the standby has to
      -- tell the primary what it is holding.
      it "keeps a read on the replica alive while the primary vacuums what it is reading" $ \pools -> do
        let people = [Person {personName = "Vacuumed " ++ show i, personAge = Just i} | i <- [1 :: Int .. 100]]
        _ <- onPrimary pools $ insertMany people
        awaitReplica pools

        onReplica pools $ do
          -- Read committed takes a new snapshot per statement and lets the
          -- old one go, so there would be nothing held across the cleanup.
          -- An application that reads twice and compares needs this.
          rawExecute "SET TRANSACTION ISOLATION LEVEL REPEATABLE READ" []

          -- Take the snapshot the cleanup below has to be kept away from.
          firstRead <- selectList ([] :: [Filter Person]) []
          liftIO $ length firstRead `shouldBe` 100

          liftIO $ do
            -- Every row gets a new version, so every old version is garbage,
            -- and the vacuum writes the cleanup that conflicts.
            onPrimary pools $ updateWhere ([] :: [Filter Person]) [PersonAge =. Just 0]
            -- VACUUM refuses to run inside a transaction block.
            runSqlPoolNoTransaction
              (rawExecute "VACUUM person" [])
              (replicatedPoolsPrimary pools)
              Nothing
            awaitReplica pools

          secondRead <- selectList ([] :: [Filter Person]) []
          liftIO $ length secondRead `shouldBe` 100
