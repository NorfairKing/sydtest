{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Syd.Yesod.E2E where

import Control.Arrow (second)
import Control.Monad.Catch
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State as State
import qualified Data.Binary.Builder as BB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Function
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Network.Socket (PortNumber)
import Test.Syd
import Test.Syd.Wai.Client (lastRequestResponseContext)
import Test.Syd.Yesod.Client
import Test.Syd.Yesod.Def
import Yesod.Core
import Yesod.Core.Unsafe (runFakeHandler)

localToE2EClient :: YesodClient site -> YesodClient (E2E site)
localToE2EClient yc =
  let uriString = "http://localhost:" <> show (yesodClientSitePort yc)
      e2e = E2E {e2eURI = uriString}
   in yc {yesodClientSite = e2e}

-- | See 'localToE2EClient'
localToE2ESpec :: YesodSpec (E2E site) -> YesodSpec site
localToE2ESpec = beforeWith (\yc -> pure $ localToE2EClient yc)

data E2E site = E2E {e2eURI :: String}
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
    pure $ TE.decodeUtf8 $ LB.toStrict $ BB.toLazyByteString pathBS -- Not safe, but it will fail during testing (if at all) so should be ok.

instance ParseRoute site => ParseRoute (E2E site) where
  parseRoute = fmap E2ERoute . parseRoute
