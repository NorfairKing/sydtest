{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Because of webdriver using dangerous constructors
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
-- For the undefined trick
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

-- | This is a helper module for 'Test.Syd.Webdriver' to let you use Yesod
-- routes to define webdriver tests.
module Test.Syd.Webdriver.Yesod
  ( -- * Defining webdriver tests with yesod
    webdriverYesodSpec,

    -- * Implementing webdriver tests with yesod
    openRoute,
    openRouteWithParams,
    getCurrentRoute,
    currentRouteShouldBe,

    -- ** Finding elements by I18N Messages
    getLinkTextI,
    findElemByLinkTextI,
    findElemByPartialLinkTextI,
    findElemByButtonTextI,
  )
where

import Control.Arrow
import Control.Monad.Reader
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Network.URI
import Test.Syd
import Test.Syd.Wai
import Test.Syd.Webdriver
import Test.Syd.Yesod
import Test.WebDriver as WD hiding (setWindowSize)
import qualified Yesod

-- | Run webdriver tests given a 'SetupFunc' for your app.
webdriverYesodSpec ::
  Yesod.YesodDispatch app =>
  (HTTP.Manager -> SetupFunc app) ->
  WebdriverSpec app ->
  Spec
webdriverYesodSpec appSetupFunc = webdriverSpec $ \man -> do
  site <- appSetupFunc man
  YesodClient {..} <- yesodClientSetupFunc man site
  pure (yesodClientSiteURI, yesodClientSite)

-- | Open a given yesod 'Route'
openRoute ::
  Yesod.RenderRoute app =>
  Route app ->
  WebdriverTestM app ()
openRoute route = openRouteWithParams route []

-- | Open a given yesod 'Route' with parameters
openRouteWithParams ::
  Yesod.RenderRoute app =>
  Route app ->
  [(Text, Text)] ->
  WebdriverTestM app ()
openRouteWithParams route extraParams = do
  let (pathPieces, queryParams) = Yesod.renderRoute route
  let q = queryTextToQuery $ map (second Just) (queryParams <> extraParams)
  let pathBSBuilder = encodePath pathPieces q
  let pathBS = LB.toStrict (BB.toLazyByteString pathBSBuilder)
  case TE.decodeUtf8' pathBS of
    Left err ->
      liftIO $
        expectationFailure $
          unlines
            [ unwords
                [ "Failed to decode path from bytestring:",
                  show pathBS
                ],
              show err
            ]
    Right t -> openPath $ T.unpack t

-- | Get the current 'Route'
getCurrentRoute ::
  Yesod.ParseRoute app =>
  WebdriverTestM app (Route app)
getCurrentRoute = do
  currentUrl <- getCurrentURL
  case parseURI currentUrl of
    Nothing -> liftIO $ expectationFailure $ "Should have been able to parse the current url into an URI: " <> currentUrl
    Just URI {..} -> do
      let (textPieces, query_) = HTTP.decodePath $ TE.encodeUtf8 $ T.pack $ concat [uriPath, uriQuery]
          queryPieces = map unJust $ HTTP.queryToQueryText query_
      case Yesod.parseRoute (textPieces, queryPieces) of
        Nothing ->
          liftIO $
            expectationFailure $
              unlines
                [ "Should have been able to parse an App route from " <> currentUrl,
                  ppShow (textPieces, queryPieces)
                ]
        Just route -> pure route
  where
    unJust (a, Just b) = (a, b)
    unJust (a, Nothing) = (a, "")

-- | Get the current 'Route' and check that it equals the given route
currentRouteShouldBe ::
  (Show (Route app), Yesod.ParseRoute app) =>
  Route app ->
  WebdriverTestM app ()
currentRouteShouldBe expected = do
  actual <- getCurrentRoute
  liftIO $ actual `shouldBe` expected

-- | Get the link text for a given I18N message.
--
-- This will only work if the language hasn't been set.
getLinkTextI :: Yesod.RenderMessage app message => message -> WebdriverTestM app Text
getLinkTextI message = do
  y <- asks webdriverTestEnvApp
  pure $ Yesod.renderMessage y [] message

-- | Find an 'Element', 'ByLinkText' the text obtained by 'getLinkTextI'
findElemByLinkTextI :: Yesod.RenderMessage app message => message -> WebdriverTestM app Element
findElemByLinkTextI message = getLinkTextI message >>= findElem . ByLinkText

-- | Find an 'Element', 'ByPartialLinkText' the text obtained by 'getLinkTextI'
findElemByPartialLinkTextI :: Yesod.RenderMessage app message => message -> WebdriverTestM app Element
findElemByPartialLinkTextI message = getLinkTextI message >>= findElem . ByPartialLinkText

-- | Find an 'Element', 'ByLinkText' the text obtained by 'getLinkTextI'
findElemByButtonTextI :: Yesod.RenderMessage app message => message -> WebdriverTestM app Element
findElemByButtonTextI message = do
  t <- getLinkTextI message
  findElem $ ByXPath $ mconcat ["//button[normalize-space()=\"", t, "\"]"]
