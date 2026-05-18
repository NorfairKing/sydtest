{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | 'GenValid' instances for the sydtest-mutation-driver configuration
-- types.  Lives in a separate package so the generators aren't compiled
-- into the executable.
module Test.Syd.Mutation.Driver.Gen () where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Path ()
import Data.GenValidity.Text ()
import Test.QuickCheck (Gen, listOf, suchThat)
import Test.Syd.Mutation.Driver.OptParse

instance Validity SuiteConfig

instance GenValid SuiteConfig

instance Validity MutationDriverConfig

instance GenValid MutationDriverConfig where
  genValid = do
    mutationDriverConfigManifests <- genValid
    mutationDriverConfigSuites <- genValid
    -- 'childMemLimit' is a String that must round-trip through JSON;
    -- unpaired surrogates cannot round-trip via aeson, so restrict the
    -- generator to text that aeson can represent.
    mutationDriverConfigChildMemLimit <- genMaybe genJsonSafeString
    mutationDriverConfigCoverageJobs <- genValid
    mutationDriverConfigCoverageRetry <- genValid
    mutationDriverConfigAugmentedManifestDir <- genValid
    mutationDriverConfigReportDir <- genValid
    mutationDriverConfigFailFast <- genValid
    pure MutationDriverConfig {..}
  shrinkValid = shrinkValidStructurally

instance Validity MutationDriverSettings

instance GenValid MutationDriverSettings where
  genValid = do
    mutationDriverSettingManifests <- genValid
    mutationDriverSettingSuites <- genValid
    mutationDriverSettingChildMemLimit <- genMaybe genJsonSafeString
    mutationDriverSettingCoverageJobs <- genValid
    mutationDriverSettingCoverageRetry <- genValid
    mutationDriverSettingAugmentedManifestDir <- genValid
    mutationDriverSettingReportDir <- genValid
    mutationDriverSettingFailFast <- genValid
    pure MutationDriverSettings {..}
  shrinkValid = shrinkValidStructurally

-- | A 'String' that contains only characters that aeson can encode and
-- decode back to the same Haskell 'Char'.  In particular this rules out
-- unpaired surrogates (U+D800..U+DFFF), which @encode . decode@ does not
-- preserve.
genJsonSafeString :: Gen String
genJsonSafeString = listOf (genValid `suchThat` (\c -> c < '\xD800' || c > '\xDFFF'))
