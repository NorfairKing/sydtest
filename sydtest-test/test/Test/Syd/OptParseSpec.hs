{-# LANGUAGE TypeApplications #-}

module Test.Syd.OptParseSpec (spec) where

import OptEnvConf.Test
import Test.Syd
import Test.Syd.OptParse

spec :: Spec
spec = do
  describe "threadsFromWord" $ do
    it "uses every capability for zero threads" $
      threadsFromWord 0 `shouldBe` ByCapabilities
    it "runs synchronously for one thread" $
      threadsFromWord 1 `shouldBe` Synchronous
    it "uses as many threads as it is given" $
      threadsFromWord 4 `shouldBe` Asynchronous 4
  describe "threadsWithNixBuildCores" $ do
    it "narrows every capability down to the cores nix gave the build" $
      threadsWithNixBuildCores 8 (Just 3) ByCapabilities `shouldBe` Asynchronous 3
    it "narrows a single core down to a synchronous run" $
      threadsWithNixBuildCores 8 (Just 1) ByCapabilities `shouldBe` Synchronous
    it "leaves every capability alone for as many cores as there are" $
      threadsWithNixBuildCores 8 (Just 8) ByCapabilities `shouldBe` ByCapabilities
    it "leaves every capability alone for more cores than there are" $
      threadsWithNixBuildCores 1 (Just 64) ByCapabilities `shouldBe` ByCapabilities
    it "leaves every capability alone for the cores of the whole machine" $
      threadsWithNixBuildCores 8 (Just 0) ByCapabilities `shouldBe` ByCapabilities
    it "leaves every capability alone without nix" $
      threadsWithNixBuildCores 8 Nothing ByCapabilities `shouldBe` ByCapabilities
    it "leaves a synchronous run alone" $
      threadsWithNixBuildCores 8 (Just 3) Synchronous `shouldBe` Synchronous
    it "leaves a configured number of threads alone" $
      threadsWithNixBuildCores 8 (Just 3) (Asynchronous 5) `shouldBe` Asynchronous 5
  describe "nixBuildCoresParser" $
    it "reads the cores nix gave the build" $
      parserEnvTest nixBuildCoresParser [("NIX_BUILD_CORES", "3")] (Just 3)
  settingsLintSpec @Settings
  goldenSettingsReferenceDocumentationSpec @Settings "test_resources/documentation.txt" "sydtest"
