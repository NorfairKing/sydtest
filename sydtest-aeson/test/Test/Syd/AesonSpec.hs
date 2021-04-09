{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.AesonSpec (spec) where

import Data.Text (Text)
import Data.Aeson as Aeson
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = do
  describe "pureGoldenAesonDocumentFile" $
    it "outputs this example the same as before" $
      pureGoldenAesonDocumentFile
        "test_resources/pure-example.json"
        $ Aeson.object 
            [ "hello" .= ( "world" :: Text ), "a" .= ( 1 :: Int ), "b" .= True ]
  describe "goldenAesonDocumentFile" $
    it "outputs this example the same as before" $
      goldenAesonDocumentFile
        "test_resources/example.json"
        $ pure $ Aeson.object [ "hello" .= ( "world" :: Text ), "a" .= ( 1 :: Int ), "b" .= True ]
