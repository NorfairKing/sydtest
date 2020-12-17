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

    /expects-header ExpectsHeaderR GET
    /expects-post-param ExpectsPostParamR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = pure "Hello, world! (GET)"

postHomeR :: Handler Html
postHomeR = pure "Hello, world! (POST)"

getExpectsHeaderR :: Handler Html
getExpectsHeaderR = do
  mh <- lookupHeader "TEST_HEADER"
  case mh of
    Nothing -> notFound
    Just _ -> pure "ok!"

getExpectsPostParamR :: Handler Html
getExpectsPostParamR = do
  mh <- lookupPostParam "TEST_HEADER"
  case mh of
    Nothing -> notFound
    Just _ -> pure "ok!"
