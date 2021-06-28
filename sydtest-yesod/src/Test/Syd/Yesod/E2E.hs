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
import Network.HTTP.Types as HTTP
import Network.URI
import Test.Syd
import Test.Syd.Wai
import Test.Syd.Yesod.Client
import Test.Syd.Yesod.Def
import Yesod.Core

yesodE2ESpec :: URI -> YesodSpec (E2E site) -> Spec
yesodE2ESpec uri =
  managerSpec
    . beforeWith'
      ( \man () -> do
          pure
            YesodClient
              { yesodClientManager = man,
                yesodClientSite = E2E,
                yesodClientSiteURI = uri
              }
      )

localToE2EClient :: YesodClient site -> YesodClient (E2E site)
localToE2EClient yc = yc {yesodClientSite = E2E}

-- | See 'localToE2EClient'
localToE2ESpec :: YesodSpec (E2E site) -> YesodSpec site
localToE2ESpec = beforeWith (\yc -> pure $ localToE2EClient yc)

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
