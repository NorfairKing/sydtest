{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.Sqitch.PersistentSpec (spec) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Database.Persist.Sql as DB
import Path (Abs, File, Path, relfile)
import qualified Path.IO as Path
import Test.Syd
import Test.Syd.Persistent.Postgresql
import Test.Syd.Sqitch
import Test.Syd.Sqitch.Persistent
import Test.Syd.Sqitch.Persistent.Example (migrateWidget)

locateSqitch :: IO (Path Abs File)
locateSqitch = do
  m <- Path.findExecutable [relfile|sqitch|]
  case m of
    Nothing -> fail "sqitch not found on PATH"
    Just p -> pure p

settingsFor :: FilePath -> Maybe Text -> IO SqitchPersistentSettings
settingsFor relDir mTag = do
  projectDir <- Path.resolveDir' relDir
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
      settings <- runIO $ settingsFor "test_resources/toy-sqitch-ok" Nothing
      sqitchPersistentPostgresqlSpec settings $
        it "passes through to the inner spec with a clean database" $ \_pool ->
          pure () :: IO ()

  describe "runSqitchPersistentChecks (negative cases)" $ do
    describe "toy-sqitch-drift" $
      persistPostgresqlSpec (pure ()) $
        itWithAll "fails when the sqitch schema is missing a column the persistent model has" $
          \(HCons tdb HNil :: HList '[TemplateDB]) (pool :: DB.ConnectionPool) -> do
            settings <- settingsFor "test_resources/toy-sqitch-drift" Nothing
            res <- try @SomeException $ runSqitchPersistentChecks settings tdb pool
            case res of
              Left _ -> pure ()
              Right () ->
                expectationFailure
                  "expected runSqitchPersistentChecks to throw when the schemas diverge"

    describe "toy-sqitch-index-drift" $
      persistPostgresqlSpec (pure ()) $
        itWithAll "fails when the columns match but the sqitch side has an extra index the persistent model does not" $
          \(HCons tdb HNil :: HList '[TemplateDB]) (pool :: DB.ConnectionPool) -> do
            settings <- settingsFor "test_resources/toy-sqitch-index-drift" Nothing
            res <- try @SomeException $ runSqitchPersistentChecks settings tdb pool
            case res of
              Left _ -> pure ()
              Right () ->
                expectationFailure
                  "expected runSqitchPersistentChecks to throw when indices diverge"
