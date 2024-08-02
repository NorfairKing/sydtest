{-# LANGUAGE TypeApplications #-}

module Test.Syd.OptParseSpec (spec) where

import OptEnvConf.Test
import Test.Syd
import Test.Syd.OptParse

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "test_resources/documentation.txt" "sydtest"
