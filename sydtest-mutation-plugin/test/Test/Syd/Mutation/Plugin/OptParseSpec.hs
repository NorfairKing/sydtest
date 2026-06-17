{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Syd.Mutation.Plugin.OptParseSpec (spec) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map as Map
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

  it "reads per-operator enable and operator-specific extra from 'operators'" $ do
    -- operators:
    --   Arith: { enable: false }
    --   ConstEmptyList: { skip-strings: true }
    let operatorsObject =
          KeyMap.fromList
            [ ("Arith", JSON.Object (KeyMap.fromList [("enable", JSON.Bool False)])),
              ("ConstEmptyList", JSON.Object (KeyMap.fromList [("skip-strings", JSON.Bool True)]))
            ]
        config = KeyMap.fromList [("operators", JSON.Object operatorsObject)]
        expectedOperators =
          Map.fromList
            [ ("Arith", OperatorConfig {operatorConfigEnable = False, operatorConfigExtra = Map.empty}),
              ( "ConstEmptyList",
                OperatorConfig
                  { operatorConfigEnable = True,
                    operatorConfigExtra = Map.fromList [("skip-strings", JSON.Bool True)]
                  }
              )
            ]
    -- Test 'parseSettings' directly: 'settingsParser' wraps it in
    -- 'withLocalYamlConfig', which loads config from a file rather than the
    -- object supplied here.
    parserTest
      parseSettings
      []
      []
      (Just config)
      defaultSettings {settingOperators = expectedOperators}
    -- And the consumed views: Arith disabled, ConstEmptyList skip-strings on.
    operatorsConfigDisabled expectedOperators `shouldBe` ["Arith"]
    operatorExtraFlag "skip-strings" (operatorConfigExtra (expectedOperators Map.! "ConstEmptyList"))
      `shouldBe` True

  it "reads a list-of-strings operator key with operatorExtraStrings" $ do
    let extra = Map.fromList [("skip-calls-to", JSON.toJSON (["max", "min"] :: [String]))]
    operatorExtraStrings "skip-calls-to" extra `shouldBe` ["max", "min"]
    -- A missing key and a non-array value both yield the empty list.
    operatorExtraStrings "absent" extra `shouldBe` []
    operatorExtraStrings "skip-calls-to" (Map.fromList [("skip-calls-to", JSON.Bool True)])
      `shouldBe` []
