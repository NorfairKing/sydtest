{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Yesod.App where

import Conduit
import Control.Monad
import qualified Data.ByteString as SB
import Web.Cookie
import Yesod

data App = App {appSessionKeyFile :: !FilePath}

mkYesod
  "App"
  [parseRoutes|

    / HomeR GET POST

    /expects-header ExpectsHeaderR GET
    /expects-get-param ExpectsGetParamR GET
    /expects-post-param ExpectsPostParamR POST
    /expects-post-body ExpectsPostBodyR   POST
    /expects-post-file ExpectsPostFileR   POST

    /redirect RedirectHomeR GET

    /set-cookie SetCookieR GET
    /expects-cookie ExpectsCookieR GET

    /form FormR GET POST
|]

instance Yesod App where
  makeSessionBackend App {..} = Just <$> defaultClientSessionBackend 30 appSessionKeyFile

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = pure "Hello, world! (GET)"

postHomeR :: Handler Html
postHomeR = pure "Hello, world! (POST)"

getExpectsHeaderR :: Handler ()
getExpectsHeaderR = do
  mh <- lookupHeader "TEST_HEADER"
  case mh of
    Nothing -> notFound
    Just _ -> pure ()

getExpectsGetParamR :: Handler ()
getExpectsGetParamR = do
  mh <- lookupGetParam "TEST_PARAM"
  case mh of
    Nothing -> notFound
    Just _ -> pure ()

postExpectsPostParamR :: Handler ()
postExpectsPostParamR = do
  mh <- lookupPostParam "TEST_PARAM"
  case mh of
    Nothing -> notFound
    Just _ -> pure ()

postExpectsPostBodyR :: Handler ()
postExpectsPostBodyR = do
  body <- SB.concat <$> runConduit (rawRequestBody .| sinkList)
  case body of
    "test" -> pure ()
    _ -> notFound

postExpectsPostFileR :: Handler ()
postExpectsPostFileR = do
  mh <- lookupFile "TEST_PARAM"
  case mh of
    Nothing -> notFound
    Just fi -> do
      unless (fileName fi == "filename") $ invalidArgs ["incorrect filename"]
      unless (fileContentType fi == "text/plain") $ invalidArgs ["incorrect content type"]
      contents <- runResourceT $ fileSourceByteString fi
      unless (contents == "test") $ invalidArgs ["incorrect body"]

getRedirectHomeR :: Handler ()
getRedirectHomeR = redirect HomeR

getSetCookieR :: Handler ()
getSetCookieR = setCookie (defaultSetCookie {setCookieName = "TEST_COOKIE"})

getExpectsCookieR :: Handler ()
getExpectsCookieR = do
  mc <- lookupCookie "TEST_COOKIE"
  case mc of
    Nothing -> notFound
    Just _ -> pure ()

getFormR :: Handler Html
getFormR = do
  (widget, enctype) <- generateFormPost $ renderDivs $ areq textField "testKey" Nothing
  defaultLayout
    [whamlet|
        <form method=post action=@{FormR} enctype=#{enctype}>
            ^{widget}
            <button>Submit
            |]

postFormR :: Handler ()
postFormR = do
  tv <- runInputPost $ ireq textField "testKey"
  unless (tv == "testVal") $ invalidArgs ["incorrect value"]
