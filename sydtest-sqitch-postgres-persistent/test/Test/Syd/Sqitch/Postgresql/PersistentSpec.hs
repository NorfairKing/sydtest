{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Sqitch.Postgresql.PersistentSpec (spec) where

import Data.Text (Text)
import Path
import Path.IO
import Test.Syd
import Test.Syd.Persistent.Postgresql (emptyPostgresOptionsSetupFunc)
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
      sqitchPersistentPostgresqlSpec settings

  describe "runSqitchPersistentChecks (negative cases)" $
    expectFailing $ do
      describe "toy-sqitch-drift" $
        setupAround emptyPostgresOptionsSetupFunc $
          it "fails when the sqitch schema is missing a column the persistent model has" $ \opts -> do
            settings <- settingsFor [reldir|test_resources/toy-sqitch-drift|] Nothing
            runSqitchPersistentChecks settings opts

      describe "toy-sqitch-index-drift" $
        setupAround emptyPostgresOptionsSetupFunc $
          it "fails when the columns match but the sqitch side has an extra index the persistent model does not" $ \opts -> do
            settings <- settingsFor [reldir|test_resources/toy-sqitch-index-drift|] Nothing
            runSqitchPersistentChecks settings opts
