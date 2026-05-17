{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.SqitchSpec (spec) where

import Control.Exception (SomeException, try)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Database.Persist.Sql as DB
import Path (Abs, File, Path, relfile)
import qualified Path.IO as Path
import Test.Syd
import Test.Syd.Persistent.Postgresql
import Test.Syd.Sqitch

-- | Locate the sqitch executable on @PATH@ at test-suite start.
locateSqitch :: IO (Path Abs File)
locateSqitch = do
  m <- Path.findExecutable [relfile|sqitch|]
  case m of
    Nothing -> fail "sqitch not found on PATH"
    Just p -> pure p

settingsFor :: FilePath -> Maybe Text -> IO SqitchSettings
settingsFor relDir mTag = do
  projectDir <- Path.resolveDir' relDir
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
      settings <- runIO $ settingsFor "test_resources/toy-sqitch-ok" Nothing
      sqitchPostgresqlSpec settings $
        it "leaves the database in an empty state after the checks pass" $ \pool -> do
          schema <- runPostgresqlTest pool querySchema
          schema `shouldBe` Map.empty

    describe "toy-sqitch-grandfathered with grandfather tag" $ do
      settings <-
        runIO $ settingsFor "test_resources/toy-sqitch-grandfathered" (Just "legacy")
      sqitchPostgresqlSpec settings $
        it "passes when the non-idempotent change is grandfathered" $ \pool -> do
          schema <- runPostgresqlTest pool querySchema
          schema `shouldBe` Map.empty

  describe "runSqitchPostgresqlChecks (negative cases)" $ do
    describe "toy-sqitch-non-idempotent" $
      persistPostgresqlSpec (pure ()) $
        itWithAll "fails because the change is non-idempotent" $
          \(HCons tdb HNil :: HList '[TemplateDB]) (pool :: DB.ConnectionPool) -> do
            settings <- settingsFor "test_resources/toy-sqitch-non-idempotent" Nothing
            res <- try @SomeException $ runSqitchPostgresqlChecks settings tdb pool
            case res of
              Left _ -> pure ()
              Right () ->
                expectationFailure
                  "expected runSqitchPostgresqlChecks to throw for a non-idempotent change"

    describe "toy-sqitch-broken-revert" $
      persistPostgresqlSpec (pure ()) $
        itWithAll "fails because the revert is not the inverse of the deploy" $
          \(HCons tdb HNil :: HList '[TemplateDB]) (pool :: DB.ConnectionPool) -> do
            settings <- settingsFor "test_resources/toy-sqitch-broken-revert" Nothing
            res <- try @SomeException $ runSqitchPostgresqlChecks settings tdb pool
            case res of
              Left _ -> pure ()
              Right () ->
                expectationFailure
                  "expected runSqitchPostgresqlChecks to throw when the round-trip check fails"

    describe "toy-sqitch-grandfathered without grandfather tag" $
      persistPostgresqlSpec (pure ()) $
        itWithAll "fails because the pre-tag non-idempotent change is no longer exempt" $
          \(HCons tdb HNil :: HList '[TemplateDB]) (pool :: DB.ConnectionPool) -> do
            settings <- settingsFor "test_resources/toy-sqitch-grandfathered" Nothing
            res <- try @SomeException $ runSqitchPostgresqlChecks settings tdb pool
            case res of
              Left _ -> pure ()
              Right () ->
                expectationFailure
                  "expected runSqitchPostgresqlChecks to throw without grandfathering"
