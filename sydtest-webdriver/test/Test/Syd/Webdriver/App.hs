{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Webdriver.App where

import Yesod

data App = App {appSessionKeyFile :: !FilePath}

mkYesod
  "App"
  [parseRoutes|

    / HomeR GET
|]

instance Yesod App where
  makeSessionBackend App {..} = Just <$> defaultClientSessionBackend 30 appSessionKeyFile

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = pure "Hello, world! (GET)"
