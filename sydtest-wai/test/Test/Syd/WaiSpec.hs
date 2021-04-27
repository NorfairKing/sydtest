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
  waiClientSpec exampleApplication $
    wit "can fetch the root and get a 200" $ do
      resp <- get "/"
      liftIO $ responseStatus resp `shouldBe` ok200
