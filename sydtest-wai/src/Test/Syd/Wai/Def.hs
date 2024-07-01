{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.Wai.Def where

import Network.HTTP.Client as HTTP
import Network.Socket (PortNumber)
import Network.URI
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Test.Syd
import Test.Syd.Wai.Client

-- | Run a given 'Wai.Application' around every test.
--
-- This provides a 'WaiClient ()' which contains the port of the running application.
waiClientSpec :: Wai.Application -> TestDefM (HTTP.Manager ': outers) (WaiClient ()) result -> TestDefM outers oldInner result
waiClientSpec application = waiClientSpecWith $ pure application

-- | Run a given 'Wai.Application', as built by the given action, around every test.
waiClientSpecWith :: IO Application -> TestDefM (HTTP.Manager ': outers) (WaiClient ()) result -> TestDefM outers oldInner result
waiClientSpecWith application = waiClientSpecWithSetupFunc (\_ _ -> liftIO $ (,) <$> application <*> pure ())

-- | Run a given 'Wai.Application', as built by the given 'SetupFunc', around every test.
waiClientSpecWithSetupFunc ::
  (HTTP.Manager -> oldInner -> SetupFunc (Application, env)) ->
  TestDefM (HTTP.Manager ': outers) (WaiClient env) result ->
  TestDefM outers oldInner result
waiClientSpecWithSetupFunc setupFunc = managerSpec . waiClientSpecWithSetupFunc' setupFunc

-- | Run a given 'Wai.Application', as built by the given 'SetupFunc', around every test.
--
-- This function doesn't set up the 'HTTP.Manager' like 'waiClientSpecWithSetupFunc' does.
waiClientSpecWithSetupFunc' ::
  (HTTP.Manager -> oldInner -> SetupFunc (Application, env)) ->
  TestDefM (HTTP.Manager ': outers) (WaiClient env) result ->
  TestDefM (HTTP.Manager ': outers) oldInner result
waiClientSpecWithSetupFunc' setupFunc = setupAroundWith' $ \man oldInner -> do
  (application, env) <- setupFunc man oldInner
  waiClientSetupFunc man application env

-- | A 'SetupFunc' for a 'WaiClient', given an 'Application' and user-defined @env@.
waiClientSetupFunc :: HTTP.Manager -> Application -> env -> SetupFunc (WaiClient env)
waiClientSetupFunc man application env = do
  p <- applicationSetupFunc application
  let uri =
        URI
          { uriScheme = "http:",
            uriAuthority =
              Just
                URIAuth
                  { uriUserInfo = "",
                    uriRegName = "localhost",
                    uriPort = ":" <> show p
                  },
            uriPath = "",
            uriQuery = "",
            uriFragment = ""
          }
  let client =
        WaiClient
          { waiClientManager = man,
            waiClientEnv = env,
            waiClientURI = uri
          }
  pure client

-- | Run a given 'Wai.Application' around every test.
--
-- This provides the port on which the application is running.
waiSpec :: Wai.Application -> TestDef outers PortNumber -> TestDef outers ()
waiSpec application = waiSpecWithSetupFunc $ pure application

-- | Run a 'Wai.Application' around every test by setting it up with the given setup function.
--
-- This provides the port on which the application is running.
waiSpecWith :: (forall r. (Application -> IO r) -> IO r) -> TestDef outers PortNumber -> TestDef outers ()
waiSpecWith appFunc = waiSpecWithSetupFunc $ SetupFunc $ \takeApp -> appFunc takeApp

-- | Run a 'Wai.Application' around every test by setting it up with the given setup function that can take an argument.
-- a
-- This provides the port on which the application is running.
waiSpecWith' :: (forall r. (Application -> IO r) -> (inner -> IO r)) -> TestDef outers PortNumber -> TestDef outers inner
waiSpecWith' appFunc = waiSpecWithSetupFunc' $ \inner -> SetupFunc $ \takeApp -> appFunc takeApp inner

-- | Run a 'Wai.Application' around every test by setting it up with the given 'SetupFunc'.
-- a
-- This provides the port on which the application is running.
waiSpecWithSetupFunc :: SetupFunc Application -> TestDef outers PortNumber -> TestDef outers ()
waiSpecWithSetupFunc setupFunc = waiSpecWithSetupFunc' $ \() -> setupFunc

-- | Run a 'Wai.Application' around every test by setting it up with the given 'SetupFunc' and inner resource.
-- a
-- This provides the port on which the application is running.
waiSpecWithSetupFunc' :: (inner -> SetupFunc Application) -> TestDef outers PortNumber -> TestDef outers inner
waiSpecWithSetupFunc' setupFunc = setupAroundWith $ \inner -> do
  application <- setupFunc inner
  applicationSetupFunc application

-- | A 'SetupFunc' to run an application and provide its port.
applicationSetupFunc :: Application -> SetupFunc PortNumber
applicationSetupFunc application = SetupFunc $ \func ->
  Warp.testWithApplication (pure application) $ \p ->
    func (fromIntegral p) -- Hopefully safe, because 'testWithApplication' should give us sensible port numbers

-- | Create a 'HTTP.Manager' before all tests in the given group.
managerSpec :: TestDefM (HTTP.Manager ': outers) inner result -> TestDefM outers inner result
managerSpec = beforeAll $ HTTP.newManager HTTP.defaultManagerSettings
