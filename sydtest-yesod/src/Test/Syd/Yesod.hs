{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Syd.Yesod
  ( -- * Functions to run a test suite
    yesodSpec,
    yesodSpecWithSiteGenerator,
    yesodSpecWithSiteGeneratorAndArgument,
    yesodSpecWithSiteSupplier,
    yesodSpecWithSiteSupplierWith,

    -- ** Core
    YesodSpec,
    YesodClient (..),
    YesodClientM (..),
    runYesodClientM,
    YesodExample,

    -- ** Helper functions in case you want to do something fancy
    yesodSpecWithFunc,

    -- * Helper functions to define tests
    yit,
    ydescribe,

    -- * Making requests
    get,
    post,
    performMethod,

    -- * Declaring assertions
    statusIs,

    -- ** Reexports
    module Network.HTTP.Types,
    module Network.HTTP.Client,
  )
where

import Control.Monad.Reader
import Control.Monad.State (MonadState, StateT (..), evalStateT)
import qualified Control.Monad.State as State
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Stack
import Network.HTTP.Client
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
import Network.Wai.Handler.Warp as Warp
import Test.Syd
import Yesod.Core as Yesod
import Yesod.Core.Unsafe

-- | Run a test suite using the given 'site'.
--
-- If your 'site' contains any resources that need to be set up, you probably want to be using one of the following functions instead.
--
-- This function exists for backward compatibility with yesod-test.
yesodSpec :: YesodDispatch site => site -> YesodSpec site -> Spec
yesodSpec site = yesodSpecWithSiteGenerator $ pure site

-- | Run a test suite using the given 'site' generator.
--
-- If your 'site' contains any resources that you will want to have set up beforhand, you will probably want to use 'yesodSpecWithSiteGeneratorAndArgument' or 'yesodSpecWithSiteSupplierWith' instead.
--
-- This function exists for backward compatibility with yesod-test.
yesodSpecWithSiteGenerator :: YesodDispatch site => IO site -> YesodSpec site -> Spec
yesodSpecWithSiteGenerator siteGen = yesodSpecWithSiteGeneratorAndArgument $ \() -> siteGen

-- | Run a test suite using the given 'site' generator which uses an inner resource.
--
-- If your 'site' contains any resources that you need to set up using a 'withX' function, you will want to use `yesodSpecWithSiteSupplier` instead.
--
-- This function exists for backward compatibility with yesod-test.
yesodSpecWithSiteGeneratorAndArgument :: YesodDispatch site => (a -> IO site) -> YesodSpec site -> SpecWith a
yesodSpecWithSiteGeneratorAndArgument func = yesodSpecWithSiteSupplierWith $ \f a -> func a >>= f

-- | Using a function that supplies a 'site', run a test suite.
yesodSpecWithSiteSupplier :: YesodDispatch site => ((site -> IO ()) -> IO ()) -> YesodSpec site -> Spec
yesodSpecWithSiteSupplier func = yesodSpecWithSiteSupplierWith (\f () -> func f)

-- | Using a function that supplies a 'site', based on an inner resource, run a test suite.
yesodSpecWithSiteSupplierWith :: YesodDispatch site => ((site -> IO ()) -> (a -> IO ())) -> YesodSpec site -> SpecWith a
yesodSpecWithSiteSupplierWith func = aroundWith func . beforeAll (newManager defaultManagerSettings) . aroundWith' yesodSpecWithFunc

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
type YesodSpec site = TestDefM '[HTTP.Manager] (YesodClient site) ()

-- | A client environment to call a Yesod app.
data YesodClient site = YesodClient
  { -- | The site itself
    yesodClientSite :: site,
    -- | The 'HTTP.Manager' to make the requests
    yesodClientManager :: HTTP.Manager,
    -- | The port that the site is running on, using @warp@
    yesodClientSitePort :: Int
  }

data YesodClientState site = YesodClientState
  { yesodClientStateLastResponse :: Maybe (Response LB.ByteString)
  }

initYesodClientState :: YesodClientState site
initYesodClientState =
  YesodClientState
    { yesodClientStateLastResponse = Nothing
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
      MonadFail
    )

-- | For backward compatibility
type YesodExample site a = YesodClientM site a

-- | Run a YesodClientM site using a YesodClient site
runYesodClientM :: YesodClient site -> YesodClientM site a -> IO a
runYesodClientM cenv (YesodClientM func) = runReaderT (evalStateT func initYesodClientState) cenv

-- | Define a test in the 'YesodClientM site' monad instead of 'IO'.
yit :: forall site. HasCallStack => String -> YesodClientM site () -> YesodSpec site
yit s f = it s ((\cenv -> runYesodClientM cenv f) :: YesodClient site -> IO ())

-- | For backward compatibility
ydescribe :: String -> YesodSpec site -> YesodSpec site
ydescribe = describe

-- | Make a @GET@ request for the given route
get :: (Yesod site, RedirectUrl site url) => url -> YesodClientM site ()
get = performMethod methodGet

-- | Make a @POST@ request for the given route
post :: (Yesod site, RedirectUrl site url) => url -> YesodClientM site ()
post = performMethod methodPost

performMethod :: (Yesod site, RedirectUrl site url) => Method -> url -> YesodClientM site ()
performMethod method route = do
  YesodClient {..} <- ask
  Right uri <-
    fmap ("http://localhost" <>)
      <$> Yesod.Core.Unsafe.runFakeHandler
        M.empty
        (const $ error "Yesod.Test: No logger available")
        yesodClientSite
        (toTextUrl route)
  resp <- liftIO $ do
    req <- parseRequest $ T.unpack uri
    httpLbs (req {method = method, port = yesodClientSitePort}) yesodClientManager
  State.modify' (\s -> s {yesodClientStateLastResponse = Just resp})

statusIs :: HasCallStack => Int -> YesodClientM site ()
statusIs i = do
  mLastResp <- State.gets yesodClientStateLastResponse
  liftIO $ case mLastResp of
    Nothing -> expectationFailure "No request made yet."
    Just r -> statusCode (responseStatus r) `shouldBe` i
