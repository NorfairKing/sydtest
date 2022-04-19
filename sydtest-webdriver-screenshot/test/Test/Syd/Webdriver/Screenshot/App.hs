{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Webdriver.Screenshot.App where

import Network.HTTP.Types as HTTP
import Network.Wai as Wai

exampleApplication :: Wai.Application
exampleApplication req sendResp =
  sendResp $
    responseLBS
      HTTP.ok200
      (requestHeaders req)
      "<html><body><h1>Hello World</h1><h2>Foo</h2><h3>Bar</h3><h4>Quux</h4></body></html>"
