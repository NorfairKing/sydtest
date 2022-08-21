{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.YesodSpec (spec) where

import Data.Text (Text)
import Network.HTTP.Client as HTTP
import Path
import Path.IO
import Test.Syd
import Test.Syd.Path
import Test.Syd.Wai (managerSpec)
import Test.Syd.Yesod
import Test.Syd.Yesod.App
import Yesod.Core

spec :: Spec
spec = managerSpec . modifyMaxSuccess (`div` 20) . yesodSpecWithSiteSetupFunc appSetupFunc $ do
  describe "Local" blogSpec
  describe "E2E" $ localToE2ESpec blogSpec

appSetupFunc :: HTTP.Manager -> SetupFunc App
appSetupFunc _ = do
  tdir <- tempDirSetupFunc "sydtest-yesod"
  sessionKeyFile <- resolveFile tdir "client_session_key.aes"
  pure $ App {appSessionKeyFile = fromAbsFile sessionKeyFile}

blogSpec :: (Yesod site, RedirectUrl site (Route App)) => YesodSpec site
blogSpec = do
  it "responds 200 OK to GET HomeR" $ do
    get HomeR
    statusShouldBe 200
  it "responds 200 OK to GET HomeR using the request builder" $ do
    request $ do
      setUrl HomeR
      setMethod "GET"
    statusShouldBe 200
  it "responds 200 OK to GET /" $ do
    get ("/" :: Text)
    statusShouldBe 200
  it "responds 200 OK to POST HomeR" $ do
    post HomeR
    statusShouldBe 200
  it "responds 200 OK to POST HomeR using the request builder" $ do
    request $ do
      setUrl HomeR
      setMethod "POST"
    statusShouldBe 200
  it "responds 200 OK to POST /" $ do
    post ("/" :: Text)
    statusShouldBe 200
  it "is able to add a header" $ do
    request $ do
      setUrl ExpectsHeaderR
      addRequestHeader ("TEST_HEADER", "test")
    statusShouldBe 200
  it "is able to add a get param" $ do
    request $ do
      setUrl ExpectsGetParamR
      addGetParam "TEST_PARAM" "test"
    statusShouldBe 200
  it "is able to add a post param" $ do
    request $ do
      setUrl ExpectsPostParamR
      setMethod "POST"
      addPostParam "TEST_PARAM" "test"
    statusShouldBe 200
  it "is able to add a raw post body" $ do
    request $ do
      setUrl ExpectsPostBodyR
      setMethod "POST"
      setRequestBody "test"
    statusShouldBe 200
  it "is able to add a post file" $ do
    request $ do
      setUrl ExpectsPostFileR
      setMethod "POST"
      addFileWith "TEST_PARAM" "filename" "test" (Just "text/plain")
    statusShouldBe 200
  it "can check for redirects" $ do
    get RedirectHomeR
    locationShouldBe HomeR
    errOrDestination <- followRedirect
    liftIO $ case errOrDestination of
      Left err -> expectationFailure (show err)
      Right _ -> pure ()
    statusShouldBe 200
  it "retains cookies" $ do
    get SetCookieR
    statusShouldBe 200
    get ExpectsCookieR
    statusShouldBe 200
  it "can do forms" $ do
    get FormR
    statusShouldBe 200
    request $ do
      setUrl FormR
      setMethod "POST"
      addToken
      addPostParam "testKey" "testVal"
    statusShouldBe 200
