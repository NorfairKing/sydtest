{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.YesodSpec (spec) where

import Data.Text (Text)
import Test.Syd
import Test.Syd.Yesod
import Test.Syd.Yesod.App

spec :: Spec
spec = yesodSpec App $ do
  yit "responds 200 OK to GET HomeR" $ do
    get HomeR
    statusIs 200
  yit "responds 200 OK to GET HomeR using the request builder" $ do
    request $ do
      setUrl HomeR
      setMethod "GET"
    statusIs 200
  yit "responds 200 OK to GET /" $ do
    get ("/" :: Text)
    statusIs 200
  yit "responds 200 OK to POST HomeR" $ do
    post HomeR
    statusIs 200
  yit "responds 200 OK to POST HomeR using the request builder" $ do
    request $ do
      setUrl HomeR
      setMethod "POST"
    statusIs 200
  yit "responds 200 OK to POST /" $ do
    post ("/" :: Text)
    statusIs 200
  yit "is able to add a header" $ do
    request $ do
      setUrl ExpectsHeaderR
      addRequestHeader ("TEST_HEADER", "test")
    statusIs 200
  yit "is able to add a post param" $ do
    request $ do
      setUrl ExpectsHeaderR
      addPostParam "TEST_HEADER" "test"
    statusIs 200
