{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.WaiSpec (spec) where

import Network.HTTP.Client as HTTP
import Test.Syd
import Test.Syd.Wai
import Test.Syd.Wai.Example

spec :: Spec
spec = managerSpec $ do
  waiSpec exampleApplication $ do
    itWithBoth "echos this example" $ \man p -> do
      let body = "hello world"
      req <- (\r -> r {port = p, requestBody = RequestBodyLBS body}) <$> parseRequest "http://localhost"
      resp <- httpLbs req man
      responseBody resp `shouldBe` body
  waiClientSpec exampleApplication $ do
    describe "get" $ do
      wit "can GET the root and get a 200" $ do
        resp <- get "/"
        liftIO $ responseStatus resp `shouldBe` ok200
      wit "can GET the /redirect and get a 303 with a location header" $ do
        resp <- get "/redirect"
        liftIO $ case lookup "Location" $ responseHeaders resp of
          Nothing -> expectationFailure "should have found a location header"
          Just l -> l `shouldBe` "/"
      wit "carries cookies correctly" $ do
        _ <- get "/set-cookie"
        resp <- get "/expects-cookie"
        liftIO $ responseStatus resp `shouldBe` ok200
    describe "post" $
      wit "can POST the root and get a 200" $ do
        resp <- post "/" "hello world"
        liftIO $ responseStatus resp `shouldBe` ok200
        liftIO $ responseBody resp `shouldBe` "hello world"
    describe "put" $
      wit "can PUT the root and get a 200" $ do
        resp <- put "/" "hello world"
        liftIO $ responseStatus resp `shouldBe` ok200
        liftIO $ responseBody resp `shouldBe` "hello world"
    describe "patch" $
      wit "can PATCH the root and get a 200" $ do
        resp <- patch "/" "hello world"
        liftIO $ responseStatus resp `shouldBe` ok200
        liftIO $ responseBody resp `shouldBe` "hello world"
    describe "options" $
      wit "can OPTIONS the root and get a 200" $ do
        resp <- options "/"
        liftIO $ responseStatus resp `shouldBe` ok200
    describe "request" $
      wit "can make a weird request" $ do
        resp <- request "HELLO" "" [] ""
        liftIO $ responseStatus resp `shouldBe` ok200
    describe "shouldRespondWith" $ do
      wit "works with a number" $
        get "/" `shouldRespondWith` 200
      wit "works with a string" $
        get "/" `shouldRespondWith` ""
