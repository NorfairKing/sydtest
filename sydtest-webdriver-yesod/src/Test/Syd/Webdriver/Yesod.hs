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

module Test.Syd.Webdriver.Yesod where

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

openRoute ::
  Yesod.RenderRoute app =>
  Route app ->
  WebdriverTestM app ()
openRoute route = openRouteWithParams route []

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

webdriverYesodSpec ::
  Yesod.YesodDispatch app =>
  (HTTP.Manager -> SetupFunc app) ->
  WebdriverSpec app ->
  Spec
webdriverYesodSpec appSetupFunc = webdriverSpec $ \man -> do
  site <- appSetupFunc man
  YesodClient {..} <- yesodClientSetupFunc man site
  pure (yesodClientSiteURI, yesodClientSite)
