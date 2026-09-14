{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Persistent.Postgresql.StandbySpec (spec) where

import Data.Int (Int64)
import Database.Persist
import Database.Persist.Sql (Single (..), rawSql, runSqlPool)
import Database.PostgreSQL.Simple (SqlError (..))
import Database.Postgres.Temp (toConnectionOptions)
import Test.Syd
import Test.Syd.Persistent.Example
import Test.Syd.Persistent.Postgresql

spec :: Spec
spec = do
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

  describe "lazyStandbySetupFunc" $ do
    -- What a suite pays for a replica it never reads: nothing.
    it "starts no standby until one is asked for" $
      unSetupFunc (postgresqlServerSetupFuncWith replicationPrimaryConfig) $ \db ->
        unSetupFunc (lazyStandbySetupFunc defaultReplicaConfig db) $ \getStandby ->
          unSetupFunc (postgresqlPoolSetupFunc (toConnectionOptions db)) $ \pool -> do
            let streaming :: IO Int64
                streaming = do
                  rows <- runSqlPool (rawSql "SELECT count(*) FROM pg_stat_replication" []) pool
                  case rows of
                    [Single n] -> pure n
                    _ -> expectationFailure "Expected exactly one row."
            streaming `shouldReturn` 0
            standby <- getStandby
            -- The same standby every time, rather than one per ask.
            standby' <- getStandby
            standbyDataDirectory standby' `shouldBe` standbyDataDirectory standby
