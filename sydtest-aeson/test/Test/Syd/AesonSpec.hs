{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.AesonSpec (spec) where

import Data.Aeson as JSON
import Data.Text (Text)
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  describe "pureGoldenJSONFile" $
    it "outputs this example the same as before" $
      pureGoldenJSONFile
        "test_resources/pure-example.json"
        $ JSON.object
          ["hello" .= ("world" :: Text), "a" .= (1 :: Int), "b" .= True]
  describe "goldenJSONFile" $
    it "outputs this example the same as before" $
      goldenJSONFile
        "test_resources/example.json"
        $ pure $ JSON.object ["hello" .= ("world" :: Text), "a" .= (1 :: Int), "b" .= True]
  describe "pureGoldenJSONValueFile" $
    it "outputs this example the same as before" $
      pureGoldenJSONValueFile
        "test_resources/pure-example.json"
        $ JSON.object
          ["hello" .= ("world" :: Text), "a" .= (1 :: Int), "b" .= True]
  describe "goldenJSONValueFile" $
    it "outputs this example the same as before" $
      goldenJSONValueFile
        "test_resources/example.json"
        $ pure $ JSON.object ["hello" .= ("world" :: Text), "a" .= (1 :: Int), "b" .= True]
