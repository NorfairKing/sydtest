{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Sqitch.PostgresqlSpec (spec) where

import Data.Text (Text)
import Path
import Path.IO
import Test.Syd
import Test.Syd.Persistent.Postgresql (emptyPostgresOptionsSetupFunc)
import Test.Syd.Sqitch.Postgresql

-- | Locate the sqitch executable on @PATH@ at test-suite start.
locateSqitch :: IO (Path Abs File)
locateSqitch = do
  m <- findExecutable [relfile|sqitch|]
  case m of
    Nothing -> fail "sqitch not found on PATH"
    Just p -> pure p

settingsFor :: Path Rel Dir -> Maybe Text -> IO SqitchSettings
settingsFor relDir mTag = do
  projectDir <- makeAbsolute relDir
  binPath <- locateSqitch
  pure
    SqitchSettings
      { sqitchSettingsProjectDir = projectDir,
        sqitchSettingsBin = binPath,
        sqitchSettingsGrandfatherTag = mTag
      }

spec :: Spec
spec = sequential $ do
  describe "sqitchPostgresqlSpec" $ do
    describe "toy-sqitch-ok" $ do
      settings <- runIO $ settingsFor [reldir|test_resources/toy-sqitch-ok|] Nothing
      sqitchPostgresqlSpec settings $
        it
          "completes without surfacing anything to downstream tests"
          (pure () :: IO ())

    describe "toy-sqitch-grandfathered with grandfather tag" $ do
      settings <-
        runIO $ settingsFor [reldir|test_resources/toy-sqitch-grandfathered|] (Just "legacy")
      sqitchPostgresqlSpec settings $
        it
          "passes when the pre-tag legacy changes are skipped"
          (pure () :: IO ())

  describe "negative cases" $
    expectFailing $ do
      describe "toy-sqitch-non-idempotent" $
        setupAround emptyPostgresOptionsSetupFunc $
          it "fails because the change is non-idempotent" $ \opts -> do
            settings <- settingsFor [reldir|test_resources/toy-sqitch-non-idempotent|] Nothing
            runSqitchPerChangeChecks settings opts

      describe "toy-sqitch-broken-revert" $
        setupAround emptyPostgresOptionsSetupFunc $
          it "fails because the revert is not the inverse of the deploy" $ \opts -> do
            settings <- settingsFor [reldir|test_resources/toy-sqitch-broken-revert|] Nothing
            runSqitchPerChangeChecks settings opts

      describe "toy-sqitch-grandfathered without grandfather tag" $
        setupAround emptyPostgresOptionsSetupFunc $
          it "fails because the pre-tag legacy change is no longer exempt" $ \opts -> do
            settings <- settingsFor [reldir|test_resources/toy-sqitch-grandfathered|] Nothing
            runSqitchPerChangeChecks settings opts
