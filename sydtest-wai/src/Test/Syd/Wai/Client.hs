{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Test.Syd.Wai.Client where

import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State as State
import qualified Data.ByteString.Lazy as LB
import GHC.Generics (Generic)
import Network.HTTP.Client as HTTP
import Network.Socket (PortNumber)
import Test.Syd

-- | A client environment for a 'Wai.Application' with a user-defined environment as well
data WaiClient env = WaiClient
  { -- The 'HTTP.Manager' tto make the requests
    waiClientManager :: !HTTP.Manager,
    -- | The user-defined environment
    waiClientEnv :: !env,
    -- The port that the application is running on, using @warp@
    waiClientPort :: !PortNumber
  }
  deriving (Generic)

data WaiClientState = WaiClientState
  { -- | The last request and response pair
    waiClientStateLast :: !(Maybe (HTTP.Request, HTTP.Response LB.ByteString)),
    -- | The cookies to pass along
    waiClientStateCookies :: !CookieJar
  }
  deriving (Generic)

initWaiClientState :: WaiClientState
initWaiClientState =
  WaiClientState
    { waiClientStateLast = Nothing,
      waiClientStateCookies = createCookieJar []
    }

-- | A Wai testing monad that carries client state, information about how to call the application,
-- a user-defined environment, and everything necessary to show nice error messages.
newtype WaiClientM env a = WaiClientM
  { unWaiClientM :: StateT WaiClientState (ReaderT (WaiClient env) IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (WaiClient env),
      MonadState WaiClientState,
      MonadFail
    )

instance IsTest (WaiClientM env ()) where
  type Arg1 (WaiClientM env ()) = ()
  type Arg2 (WaiClientM env ()) = WaiClient env
  runTest func = runTest (\() -> func)

instance IsTest (outerArgs -> WaiClientM env ()) where
  type Arg1 (outerArgs -> WaiClientM env ()) = outerArgs
  type Arg2 (outerArgs -> WaiClientM env ()) = WaiClient env
  runTest func = runTest (\outerArgs waiClient -> runWaiClientM waiClient (func outerArgs))

instance IsTest (WaiClientM env (GoldenTest a)) where
  type Arg1 (WaiClientM env (GoldenTest a)) = ()
  type Arg2 (WaiClientM env (GoldenTest a)) = WaiClient env
  runTest func = runTest (\() -> func)

instance IsTest (outerArgs -> WaiClientM env (GoldenTest a)) where
  type Arg1 (outerArgs -> WaiClientM env (GoldenTest a)) = outerArgs
  type Arg2 (outerArgs -> WaiClientM env (GoldenTest a)) = WaiClient env
  runTest func = runTest (\outerArgs waiClient -> runWaiClientM waiClient (func outerArgs))

-- | For compatibility with @hspec-wai@
type WaiSession st a = WaiClientM st a

-- | For compatibility with @hspec-wai@
type WaiExpectation st = WaiSession st ()

-- | Run a WaiClientM env using a WaiClient env
runWaiClientM :: WaiClient env -> WaiClientM env a -> IO a
runWaiClientM cenv (WaiClientM func) = runReaderT (evalStateT func initWaiClientState) cenv

-- | Get the most recently sent request.
getRequest :: WaiClientM env (Maybe HTTP.Request)
getRequest = State.gets (fmap fst . waiClientStateLast)

-- | Get the most recently received response.
getResponse :: WaiClientM env (Maybe (HTTP.Response LB.ByteString))
getResponse = State.gets (fmap snd . waiClientStateLast)

-- | Get the most recently sent request and the response to it.
getLast :: WaiClientM env (Maybe (HTTP.Request, HTTP.Response LB.ByteString))
getLast = State.gets waiClientStateLast

-- | Annotate the given test code with the last request and its response, if one has been made already.
withLastRequestContext :: WaiClientM site a -> WaiClientM site a
withLastRequestContext wfunc@(WaiClientM func) = do
  mLast <- getLast
  case mLast of
    Nothing -> wfunc
    Just (req, resp) ->
      WaiClientM $ do
        s <- get
        c <- ask
        let ctx = lastRequestResponseContext req resp
        (r, s') <- liftIO $ context ctx $ runReaderT (runStateT func s) c
        put s'
        pure r

-- | An assertion context, for 'Context', that shows the last request and response
lastRequestResponseContext :: (Show respBody) => HTTP.Request -> HTTP.Response respBody -> String
lastRequestResponseContext req resp =
  unlines
    [ "last request:",
      ppShow req,
      "full response:",
      ppShow resp
    ]
