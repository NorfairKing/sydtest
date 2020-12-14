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
    / HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = pure "Hello, world!"
