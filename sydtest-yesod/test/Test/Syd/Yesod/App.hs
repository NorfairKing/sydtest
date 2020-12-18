{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Yesod.App where

import Conduit
import Control.Monad
import qualified Data.ByteString as SB
import Yesod

data App = App

mkYesod
  "App"
  [parseRoutes|
    / HomeR GET POST

    /expects-header ExpectsHeaderR GET
    /expects-post-param ExpectsPostParamR POST
    /expects-post-body ExpectsPostBodyR   POST
    /expects-post-file ExpectsPostFileR   POST
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

postExpectsPostParamR :: Handler Html
postExpectsPostParamR = do
  mh <- lookupPostParam "TEST_PARAM"
  case mh of
    Nothing -> notFound
    Just _ -> pure "ok!"

postExpectsPostBodyR :: Handler Html
postExpectsPostBodyR = do
  body <- SB.concat <$> runConduit (rawRequestBody .| sinkList)
  case body of
    "test" -> pure "ok!"
    _ -> notFound

postExpectsPostFileR :: Handler Html
postExpectsPostFileR = do
  mh <- lookupFile "TEST_HEADER"
  case mh of
    Nothing -> notFound
    Just fi -> do
      unless (fileName fi == "filename") $ invalidArgs ["incorrect filename"]
      unless (fileContentType fi == "text/plain") $ invalidArgs ["incorrect content type"]
      contents <- runResourceT $ fileSourceByteString fi
      unless (contents == "test") $ invalidArgs ["incorrect body"]
      pure "ok!"
