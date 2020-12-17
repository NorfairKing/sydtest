module Test.Syd.YesodSpec (spec) where

import Test.Syd
import Test.Syd.Yesod
import Test.Syd.Yesod.App

spec :: Spec
spec = yesodSpec App $ do
  yit "responds 200 OK to GET HomeR" $ do
    get HomeR
    statusIs 200
  yit "responds 200 OK to GET /" $ do
    get "/"
    statusIs 200
  yit "responds 200 OK to POST HomeR" $ do
    post HomeR
    statusIs 200
  yit "responds 200 OK to POST /" $ do
    post "/"
    statusIs 200
