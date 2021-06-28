{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Syd.Yesod.E2E where

import Control.Arrow (second)
import qualified Data.Binary.Builder as BB
import qualified Data.ByteString.Lazy as LB
import Data.Function
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS
import Network.HTTP.Types as HTTP
import Network.URI
import Test.Syd
import Test.Syd.Yesod.Client
import Test.Syd.Yesod.Def
import Yesod.Core

-- | Run an end-to-end yesod test suite against a remote server at the given 'URI'.
--
-- If you would like to write tests that can be run against both a local and a remote instance of your site, you can use the following type:
--
-- > mySpec :: (Yesod site, RedirectUrl site (Route App)) => YesodSpec site
-- > mySpec = do
-- >   it "responds 200 OK to GET HomeR" $ do
-- >     get HomeR
-- >     statusIs 200
yesodE2ESpec :: URI -> YesodSpec (E2E site) -> Spec
yesodE2ESpec uri = beforeAll newTlsManager . yesodE2ESpec' uri

-- | Like 'yesodE2ESpec', but doesn't set up the 'HTTP.Manager' for you.
--
-- If you are running the end-to-end test against a server that uses
-- @https://@, make sure to use a TLS-enabled 'HTTP.Manager'.
--
-- You can do this using @beforeAll newTlsManager@.
yesodE2ESpec' :: URI -> YesodSpec (E2E site) -> TestDef '[HTTP.Manager] ()
yesodE2ESpec' uri =
  beforeWith'
    ( \man () -> do
        pure
          YesodClient
            { yesodClientManager = man,
              yesodClientSite = E2E,
              yesodClientSiteURI = uri
            }
    )

-- | Turn a local 'YesodClient site' into a remote 'YesodClient (E2E site)'.
localToE2EClient :: YesodClient site -> YesodClient (E2E site)
localToE2EClient yc = yc {yesodClientSite = E2E}

-- | See 'localToE2EClient'
--
-- Turn an end-to-end yesod test suite into a local yesod test suite by
-- treating a local instance as remote.
localToE2ESpec :: YesodSpec (E2E site) -> YesodSpec site
localToE2ESpec = beforeWith (\yc -> pure $ localToE2EClient yc)

-- | A dummy type that is an instance of 'Yesod', with as a phantom type, the app that it represents.
--
-- That is to say, @E2E site@ is an instance of 'Yesod' that pretends to be a
-- @site@. You can treat it as a @site@ in end-to-end tests, except that you
-- cannot use the @site@ value because there is none in there.
data E2E site = E2E
  deriving (Show, Eq, Generic)

instance Yesod site => Yesod (E2E site)

instance (Eq (Route site), RenderRoute site) => RenderRoute (E2E site) where
  data Route (E2E site) = E2ERoute {unE2ERoute :: Route site}
    deriving (Generic)
  renderRoute (E2ERoute route) = renderRoute route

instance Show (Route site) => Show (Route (E2E site)) where
  show = show . unE2ERoute

instance Eq (Route site) => Eq (Route (E2E site)) where
  (==) = (==) `on` unE2ERoute

instance RenderRoute site => RedirectUrl (E2E site) (Route site) where
  toTextUrl route = do
    let (urlPieces, queryParams) = renderRoute (E2ERoute route)
        q = queryTextToQuery $ map (second Just) queryParams
        pathBS = encodePath urlPieces q
    pure $ TE.decodeUtf8 (LB.toStrict (BB.toLazyByteString pathBS)) -- Not safe, but it will fail during testing (if at all) so should be ok.

instance ParseRoute site => ParseRoute (E2E site) where
  parseRoute = fmap E2ERoute . parseRoute
