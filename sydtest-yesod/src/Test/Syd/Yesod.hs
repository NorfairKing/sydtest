{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Syd.Yesod
  ( yesodSpec,
    yesodSpecWith,
    yesodSpecWithFunc,
    YesodSpec,
    YesodClient (..),
    YesodClientM (..),
    runYesodClientM,

    -- ** Helper functions to define tests
    yit,
    ydescribe,

    -- ** Making requests
    get,

    -- ** Reexports
    module Network.HTTP.Types,
    module Network.HTTP.Client,
  )
where

import Control.Monad.Reader
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import GHC.Stack
import Network.HTTP.Client
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
import Network.Wai.Handler.Warp as Warp
import Test.Syd
import Yesod.Core as Yesod

-- | Using a function that supplies a 'site', run a test suite.
yesodSpec :: YesodDispatch site => ((site -> IO ()) -> IO ()) -> YesodSpec site -> Spec
yesodSpec func = yesodSpecWith (\f () -> func f)

-- | Using a function that supplies a 'site', based on an inner resource, run a test suite.
yesodSpecWith :: YesodDispatch site => ((site -> IO ()) -> (a -> IO ())) -> YesodSpec site -> SpecWith a
yesodSpecWith func = aroundWith func . beforeAll (newManager defaultManagerSettings) . aroundWith' yesodSpecWithFunc

-- | Turn a function that takes a 'YesodClient site' into a function that only takes a 'site'.
yesodSpecWithFunc :: YesodDispatch site => (HTTP.Manager -> YesodClient site -> IO ()) -> (HTTP.Manager -> site -> IO ())
yesodSpecWithFunc func man site =
  Warp.testWithApplication (Yesod.toWaiAppPlain site) $ \port ->
    let client =
          YesodClient
            { yesodClientManager = man,
              yesodClientSite = site,
              yesodClientSitePort = port
            }
     in func man client

-- | For backward compatibility with yesod-test
type YesodSpec site = forall l. TestDefM (HTTP.Manager ': l) (YesodClient site) ()

-- | A client environment to call a Yesod app.
data YesodClient site = YesodClient
  { yesodClientSite :: site,
    yesodClientManager :: HTTP.Manager,
    yesodClientSitePort :: Int
  }

-- | A monad to call a Yesod app.
--
-- This has access to a 'YesodClient site'.
newtype YesodClientM site a = YesodClientM {unYesodClientM :: ReaderT (YesodClient site) IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (YesodClient site))

-- | Run a YesodClientM site using a YesodClient site
runYesodClientM :: YesodClient site -> YesodClientM site a -> IO a
runYesodClientM cenv (YesodClientM rFunc) = runReaderT rFunc cenv

-- | Define a test in the 'YesodClientM site' monad instead of 'IO'.
yit :: forall site. HasCallStack => String -> YesodClientM site () -> YesodSpec site
yit s f = it s ((\cenv -> runYesodClientM cenv f) :: YesodClient site -> IO ())

-- | For backward compatibility
ydescribe :: String -> YesodSpec site -> YesodSpec site
ydescribe = describe

get :: Yesod site => Route site -> YesodClientM site (Response LB.ByteString)
get route = do
  YesodClient {..} <- ask
  let uri = yesodRender yesodClientSite "http://localhost" route []
  liftIO $ do
    req <- parseRequest $ T.unpack uri
    httpLbs (req {port = yesodClientSitePort}) yesodClientManager
