module Test.Syd.Webdriver.Screenshot.App where

import Network.HTTP.Types as HTTP
import Network.Wai as Wai

exampleApplication :: Wai.Application
exampleApplication req sendResp = do
  lb <- strictRequestBody req
  sendResp $ responseLBS HTTP.ok200 (requestHeaders req) lb
