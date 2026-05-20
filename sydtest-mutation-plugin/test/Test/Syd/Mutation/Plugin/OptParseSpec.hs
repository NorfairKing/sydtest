{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.Mutation.Plugin.OptParseSpec (spec) where

import OptEnvConf.Test
import Path
import Test.Syd
import Test.Syd.Mutation.Plugin.OptParse

spec :: Spec
spec = describe "Settings parser" $ do
  settingsLintSpec @Settings

  it "produces defaults from empty inputs" $
    settingsParserTest @Settings [] [] Nothing defaultSettings

  it "reads 'ignore' from --ignore=A,B" $
    settingsParserTest @Settings
      ["--ignore=logDebug,logInfo"]
      []
      Nothing
      defaultSettings {settingIgnore = ["logDebug", "logInfo"]}

  it "reads 'ignore' from MUTATION_PLUGIN_IGNORE" $
    settingsParserTest @Settings
      []
      [("MUTATION_PLUGIN_IGNORE", "logDebug,logInfo")]
      Nothing
      defaultSettings {settingIgnore = ["logDebug", "logInfo"]}

  it "reads --skip" $
    settingsParserTest @Settings
      ["--skip"]
      []
      Nothing
      defaultSettings {settingSkipInstrumentation = True}

  it "reads MUTATION_PLUGIN_MANIFEST_DIR" $ do
    dir <- parseAbsDir "/tmp/foo/"
    settingsParserTest @Settings
      []
      [("MUTATION_PLUGIN_MANIFEST_DIR", "/tmp/foo/")]
      Nothing
      defaultSettings {settingManifestDir = Just dir}
