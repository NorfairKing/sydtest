{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.Sqitch.Postgresql.PersistentSpec (spec) where

import Data.Text (Text)
import qualified Database.Persist.Sql as DB
import Path
import Path.IO
import Test.Syd
import Test.Syd.Persistent.Postgresql
import Test.Syd.Sqitch.Postgresql
import Test.Syd.Sqitch.Postgresql.Persistent
import Test.Syd.Sqitch.Postgresql.Persistent.Example (migrateWidget)

locateSqitch :: IO (Path Abs File)
locateSqitch = do
  m <- findExecutable [relfile|sqitch|]
  case m of
    Nothing -> fail "sqitch not found on PATH"
    Just p -> pure p

settingsFor :: Path Rel Dir -> Maybe Text -> IO SqitchPersistentSettings
settingsFor relDir mTag = do
  projectDir <- makeAbsolute relDir
  binPath <- locateSqitch
  pure
    SqitchPersistentSettings
      { sqitchPersistentSqitch =
          SqitchSettings
            { sqitchSettingsProjectDir = projectDir,
              sqitchSettingsBin = binPath,
              sqitchSettingsGrandfatherTag = mTag
            },
        sqitchPersistentAutoMigration = migrateWidget,
        sqitchPersistentExtraSetup = pure ()
      }

spec :: Spec
spec = sequential $ do
  describe "sqitchPersistentPostgresqlSpec" $ do
    describe "toy-sqitch-ok" $ do
      settings <- runIO $ settingsFor [reldir|test_resources/toy-sqitch-ok|] Nothing
      sqitchPersistentPostgresqlSpec settings $
        it "passes through to the inner spec with a clean database" $ \_pool ->
          pure () :: IO ()

  describe "runSqitchPersistentChecks (negative cases)" $
    expectFailing $ do
      describe "toy-sqitch-drift" $
        persistPostgresqlSpec (pure ()) $
          itWithAll "fails when the sqitch schema is missing a column the persistent model has" $
            \(HCons tdb HNil :: HList '[TemplateDB]) (pool :: DB.ConnectionPool) -> do
              settings <- settingsFor [reldir|test_resources/toy-sqitch-drift|] Nothing
              runSqitchPersistentChecks settings tdb pool

      describe "toy-sqitch-index-drift" $
        persistPostgresqlSpec (pure ()) $
          itWithAll "fails when the columns match but the sqitch side has an extra index the persistent model does not" $
            \(HCons tdb HNil :: HList '[TemplateDB]) (pool :: DB.ConnectionPool) -> do
              settings <- settingsFor [reldir|test_resources/toy-sqitch-index-drift|] Nothing
              runSqitchPersistentChecks settings tdb pool
