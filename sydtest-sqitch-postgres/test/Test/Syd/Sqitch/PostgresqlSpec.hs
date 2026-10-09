{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Sqitch.PostgresqlSpec (spec) where

import Data.Text (Text)
import Path
import Path.IO
import Test.Syd
import Test.Syd.OptParse (Settings (..), Timeout (..), defaultSettings)
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
    -- Both tests do work proportional to the number of changes, so a
    -- fixed budget would shrink as the plan grows.
    it "gives the sqitch tests a timeout that scales with the number of changes" $ do
      settings <- settingsFor [reldir|test_resources/toy-sqitch-ok|] Nothing
      forest <-
        execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $
          sqitchPostgresqlSpec settings
      -- Both tests have to be inside the node, not merely beside it.
      case forest of
        [ DefDescribeNode
            _
            [ DefTimeoutNode
                scaleTimeout
                [DefSpecifyNode _ _ _, DefSpecifyNode _ _ _]
              ]
          ] ->
            map scaleTimeout [DoNotTimeout, TimeoutAfterMicros 1_000_000]
              `shouldBe` [DoNotTimeout, TimeoutAfterMicros 3_000_000]
        _ -> expectationFailure "expected both sqitch tests to sit under a timeout node"

    describe "toy-sqitch-ok" $ do
      settings <- runIO $ settingsFor [reldir|test_resources/toy-sqitch-ok|] Nothing
      sqitchPostgresqlSpec settings

    describe "toy-sqitch-grandfathered with grandfather tag" $ do
      settings <-
        runIO $ settingsFor [reldir|test_resources/toy-sqitch-grandfathered|] (Just "legacy")
      sqitchPostgresqlSpec settings

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

      describe "toy-sqitch-hardcoded-public" $
        setupAround emptyPostgresOptionsSetupFunc $
          it "fails because a verify hardcodes the public schema (caught by deploying into a non-public schema)" $ \opts -> do
            settings <- settingsFor [reldir|test_resources/toy-sqitch-hardcoded-public|] Nothing
            runSqitchPerChangeChecks settings opts
