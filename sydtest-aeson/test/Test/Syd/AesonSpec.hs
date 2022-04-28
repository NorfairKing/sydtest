{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.AesonSpec (spec) where

import Control.Exception
import Control.Monad
import Data.Aeson as JSON
import Data.Text (Text)
import System.Directory (doesFileExist, removeFile)
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  describe "pureGoldenJSONFile" $ do
    it "outputs this example the same as before" $
      pureGoldenJSONFile
        "test_resources/pure-example.json"
        $ JSON.object
          ["hello" .= ("world" :: Text), "a" .= (1 :: Int), "b" .= True]

    it "does not write an empty file if encoding fails" $ do
      let path = "test_resources/pure-example-exception.json"
      let result = JSON.object ["undefined" .= (undefined :: Text)]
      let goldenTest = pureGoldenJSONFile path result
      -- Make sure there is nothing in the way
      existsBefore <- doesFileExist path
      when existsBefore $ removeFile path

      -- Try to write, this will fail
      goldenTestWrite goldenTest result `catch` (\(_ :: ErrorCall) -> pure ())

      -- Make sure there is no file afterwards
      exists <- doesFileExist path
      exists `shouldBe` False

  describe "goldenJSONFile" $
    it "outputs this example the same as before" $
      goldenJSONFile
        "test_resources/example.json"
        $ pure $ JSON.object ["hello" .= ("world" :: Text), "a" .= (1 :: Int), "b" .= True]

  describe "pureGoldenJSONValueFile" $ do
    it "outputs this example the same as before" $
      pureGoldenJSONValueFile
        "test_resources/pure-example-value.json"
        $ JSON.object
          ["hello" .= ("world" :: Text), "a" .= (1 :: Int), "b" .= True]

    it "does not write an empty file if encoding fails" $ do
      let path = "test_resources/pure-example-value-exception.json"
      let result = JSON.object ["undefined" .= (undefined :: Text)]
      let goldenTest = pureGoldenJSONValueFile path result
      -- Make sure there is nothing in the way
      existsBefore <- doesFileExist path
      when existsBefore $ removeFile path

      -- Try to write, this will fail
      goldenTestWrite goldenTest result `catch` (\(_ :: ErrorCall) -> pure ())

      -- Make sure there is no file afterwards
      existsAfter <- doesFileExist path
      existsAfter `shouldBe` False

  describe "goldenJSONValueFile" $
    it "outputs this example the same as before" $
      goldenJSONValueFile
        "test_resources/example-value.json"
        $ pure $ JSON.object ["hello" .= ("world" :: Text), "a" .= (1 :: Int), "b" .= True]
