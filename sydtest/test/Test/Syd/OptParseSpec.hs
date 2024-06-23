{-# LANGUAGE TypeApplications #-}

module Test.Syd.OptParseSpec (spec) where

import OptEnvConf.Test
import Test.Syd
import Test.Syd.OptParse

spec :: Spec
spec = do
  settingsLintSpec @Settings
  goldenReferenceDocumentationSpec @Settings "test_resources/documentation.txt" "sydtest"
