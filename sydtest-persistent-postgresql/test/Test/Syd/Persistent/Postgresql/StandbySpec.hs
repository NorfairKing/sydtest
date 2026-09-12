{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Persistent.Postgresql.StandbySpec (spec) where

import Database.Persist
import Database.PostgreSQL.Simple (SqlError (..))
import Test.Syd
import Test.Syd.Persistent.Example
import Test.Syd.Persistent.Postgresql

-- | Far enough behind for a test to tell, without being a lag no real replica
-- would show.
standbyConfig :: StandbyConfig
standbyConfig =
  StandbyConfig
    { standbyConfigMinApplyDelay = Just "500ms"
    }

spec :: Spec
spec =
  persistPostgresqlAdminSpecWith replicationPrimaryConfig migrateExample $
    setupAroundAllWithAll (\outers -> postgresqlStandbySetupFunc standbyConfig (fst (getElem outers :: TemplateDB))) $
      setupAroundWithAll (\outers () -> replicatedPoolsSetupFunc (getElem outers) (getElem outers)) $
        describe "postgresqlStandbySetupFunc" $ do
          it "does not show a write on the standby while it is behind" $ \pools -> do
            let p = Person {personName = "John Doe", personAge = Just 21}
            i <- runPostgresqlTest (replicatedPoolsPrimary pools) $ insert p
            mp <- runPostgresqlTest (replicatedPoolsStandby pools) $ get i
            mp `shouldBe` Nothing

          it "shows the write on the standby once it has caught up" $ \pools -> do
            let p = Person {personName = "Jane Doe", personAge = Just 22}
            i <- runPostgresqlTest (replicatedPoolsPrimary pools) $ insert p
            replicatedPoolsAwaitCaughtUp pools
            mp <- runPostgresqlTest (replicatedPoolsStandby pools) $ get i
            mp `shouldBe` Just p

          -- A hot standby is read-only because postgres refuses the write, not
          -- because the test was told to pretend.
          it "refuses a write through the standby" $ \pools -> do
            let p = Person {personName = "Jim Doe", personAge = Just 23}
            runPostgresqlTest (replicatedPoolsStandby pools) (insert_ p)
              `shouldThrow` (\(e :: SqlError) -> sqlState e == "25006")
