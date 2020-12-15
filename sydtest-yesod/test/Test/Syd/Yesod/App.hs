{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Yesod.App where

import Yesod

data App = App

mkYesod
  "App"
  [parseRoutes|
    / HomeR GET POST
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = pure "Hello, world! (GET)"

postHomeR :: Handler Html
postHomeR = pure "Hello, world! (POST)"
