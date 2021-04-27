{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Wai.Example where

import Network.HTTP.Types as HTTP
import Network.Wai as Wai

exampleApplication :: Wai.Application
exampleApplication req sendResp = do
  case pathInfo req of
    ["expects-header"] -> do
      let status = case lookup "TEST_HEADER" $ requestHeaders req of
            Nothing -> HTTP.notFound404
            Just _ -> HTTP.ok200
      sendResp $ responseLBS status [] ""
    ["redirect"] ->
      sendResp $ responseLBS HTTP.seeOther303 [("Location", "/")] ""
    ["set-cookie"] ->
      sendResp $ responseLBS HTTP.ok200 [("Set-Cookie", "hello=world")] ""
    ["expects-cookie"] -> do
      let status = case lookup "Cookie" $ requestHeaders req of
            Nothing -> HTTP.notFound404
            Just _ -> HTTP.ok200
      sendResp $ responseLBS status [] ""
    _ -> do
      lb <- strictRequestBody req
      sendResp $ responseLBS HTTP.ok200 (requestHeaders req) lb
