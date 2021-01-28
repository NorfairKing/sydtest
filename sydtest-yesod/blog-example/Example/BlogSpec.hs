module Example.BlogSpec (spec) where

import Example.Blog
import Test.Syd
import Test.Syd.Yesod

spec :: Spec
spec = yesodSpec App $ do
  yit "can GET the home page" $ do
    get HomeR
    statusIs 200
