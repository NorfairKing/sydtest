module Test.Syd.YesodSpec (spec) where

import Network.HTTP.Types.Status
import Test.Syd
import Test.Syd.Yesod
import Test.Syd.Yesod.App

spec :: Spec
spec = yesodSpec App $ do
  yit "responds 200 OK to GET HomeR" $ do
    r <- get HomeR
    liftIO $ responseStatus r `shouldBe` ok200
  yit "responds 200 OK to GET /" $ do
    r <- get "/"
    liftIO $ responseStatus r `shouldBe` ok200
  yit "responds 200 OK to POST HomeR" $ do
    r <- post HomeR
    liftIO $ responseStatus r `shouldBe` ok200
  yit "responds 200 OK to POST /" $ do
    r <- post "/"
    liftIO $ responseStatus r `shouldBe` ok200
