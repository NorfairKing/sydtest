{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Syd.Yesod.Client where

import qualified Blaze.ByteString.Builder as Builder
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State (MonadState, StateT (..), evalStateT, execStateT)
import qualified Control.Monad.State as State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Stack
import Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Network.Wai.Handler.Warp as Warp
import Test.Syd
import Web.Cookie as Cookie
import Yesod.Core as Yesod
import Yesod.Core.Unsafe

-- | A client environment to call a Yesod app.
data YesodClient site = YesodClient
  { -- | The site itself
    yesodClientSite :: !site,
    -- | The 'HTTP.Manager' to make the requests
    yesodClientManager :: !HTTP.Manager,
    -- | The port that the site is running on, using @warp@
    yesodClientSitePort :: !Int
  }

data YesodClientState site = YesodClientState
  { -- | The last response received
    yesodClientStateLastResponse :: !(Maybe (Response LB.ByteString)),
    -- | The cookies to pass along
    yesodClientStateCookies :: !Cookies
  }

initYesodClientState :: YesodClientState site
initYesodClientState =
  YesodClientState
    { yesodClientStateLastResponse = Nothing,
      yesodClientStateCookies = []
    }

-- | A monad to call a Yesod app.
--
-- This has access to a 'YesodClient site'.
newtype YesodClientM site a = YesodClientM
  { unYesodClientM :: StateT (YesodClientState site) (ReaderT (YesodClient site) IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (YesodClient site),
      MonadState (YesodClientState site),
      MonadFail,
      MonadThrow
    )

-- | For backward compatibility
type YesodExample site a = YesodClientM site a

-- | Run a YesodClientM site using a YesodClient site
runYesodClientM :: YesodClient site -> YesodClientM site a -> IO a
runYesodClientM cenv (YesodClientM func) = runReaderT (evalStateT func initYesodClientState) cenv

getResponse :: YesodClientM site (Maybe (Response LB.ByteString))
getResponse = State.gets yesodClientStateLastResponse

getLocation :: ParseRoute site => YesodClientM site (Either Text (Route site))
getLocation = do
  mr <- getResponse
  case mr of
    Nothing -> return $ Left "getLocation called, but there was no previous response, so no Location header"
    Just r -> case lookup "Location" (responseHeaders r) of
      Nothing -> return $ Left "getLocation called, but the previous response has no Location header"
      Just h -> case parseRoute $ decodePath h of
        Nothing -> return $ Left "getLocation called, but couldn’t parse it into a route"
        Just l -> return $ Right l
  where
    decodePath b =
      let (x, y) = SB8.break (== '?') b
       in (HTTP.decodePathSegments x, unJust <$> HTTP.parseQueryText y)
    unJust (a, Just b) = (a, b)
    unJust (a, Nothing) = (a, mempty)
