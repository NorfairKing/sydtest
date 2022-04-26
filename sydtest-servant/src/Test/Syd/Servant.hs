{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Syd.Servant
  ( servantSpec,
    servantSpecWithSetupFunc,
    clientEnvSetupFunc,
    servantSpecWithContext,
    servantSpecWithSetupFuncWithContext,
    clientEnvSetupFuncWithContext,
    testClient,
    testClientOrError,
  )
where

import Data.Kind
import Network.HTTP.Client as HTTP
import Servant
import Servant.Client
import Test.Syd
import Test.Syd.Wai

type ServantSpec = TestDef '[HTTP.Manager] ClientEnv

-- | Run a given servant server around every test
servantSpec :: forall api. HasServer api '[] => Servant.Proxy api -> ServerT api Handler -> ServantSpec -> Spec
servantSpec py server = servantSpecWithSetupFunc py (pure server)

-- | Run a servant server around every test, based around the given 'SetupFunc'
servantSpecWithSetupFunc :: forall api. HasServer api '[] => Servant.Proxy api -> SetupFunc (ServerT api Handler) -> ServantSpec -> Spec
servantSpecWithSetupFunc py setupFunc = servantSpecWithSetupFunc' py $ \() -> setupFunc

servantSpecWithSetupFunc' :: forall api inner. HasServer api '[] => Servant.Proxy api -> (inner -> SetupFunc (ServerT api Handler)) -> ServantSpec -> SpecWith inner
servantSpecWithSetupFunc' py serverSetupFunc = managerSpec . setupAroundWith' (\man inner -> serverSetupFunc inner >>= clientEnvSetupFunc py man)

clientEnvSetupFunc :: forall api. HasServer api '[] => Servant.Proxy api -> HTTP.Manager -> ServerT api Handler -> SetupFunc ClientEnv
clientEnvSetupFunc py man server = do
  let application = serve py server
  p <- applicationSetupFunc application
  pure $
    mkClientEnv
      man
      ( BaseUrl
          Http
          "127.0.0.1"
          (fromIntegral p) -- Safe because it is PortNumber -> Int
          ""
      )

-- | Like 'servantSpec', but allows setting a context. Useful for example when your server uses basic auth.
servantSpecWithContext ::
  forall api (ctx :: [Type]).
  (HasServer api ctx, HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters) =>
  Servant.Proxy api ->
  Context ctx ->
  ServerT api Handler ->
  ServantSpec ->
  Spec
servantSpecWithContext py ctx server = servantSpecWithSetupFuncWithContext py ctx (pure server)

-- | Like 'servantSpecWithSetupFunc', but allows setting a context. Useful for example when your server uses basic auth.
servantSpecWithSetupFuncWithContext ::
  forall api (ctx :: [Type]).
  (HasServer api ctx, HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters) =>
  Servant.Proxy api ->
  Context ctx ->
  SetupFunc (ServerT api Handler) ->
  ServantSpec ->
  Spec
servantSpecWithSetupFuncWithContext py ctx setupFunc = servantSpecWithSetupFuncWithContext' py ctx $ \() -> setupFunc

servantSpecWithSetupFuncWithContext' ::
  forall api (ctx :: [Type]) inner.
  (HasServer api ctx, HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters) =>
  Servant.Proxy api ->
  Context ctx ->
  (inner -> SetupFunc (ServerT api Handler)) ->
  ServantSpec ->
  SpecWith inner
servantSpecWithSetupFuncWithContext' py ctx serverSetupFunc = managerSpec . setupAroundWith' (\man inner -> serverSetupFunc inner >>= clientEnvSetupFuncWithContext py ctx man)

clientEnvSetupFuncWithContext ::
  forall api (ctx :: [Type]).
  (HasServer api ctx, HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters) =>
  Servant.Proxy api ->
  Context ctx ->
  HTTP.Manager ->
  ServerT api Handler ->
  SetupFunc ClientEnv
clientEnvSetupFuncWithContext py x man server = do
  let application = serveWithContext py x server
  p <- applicationSetupFunc application
  pure $
    mkClientEnv
      man
      ( BaseUrl
          Http
          "127.0.0.1"
          (fromIntegral p) -- Safe because it is PortNumber -> Int
          ""
      )

testClient :: ClientEnv -> ClientM a -> IO a
testClient cenv func = do
  errOrRes <- testClientOrError cenv func
  case errOrRes of
    Left err -> expectationFailure $ show err
    Right r -> pure r

#if MIN_VERSION_servant_client(0,16,0)
testClientOrError :: ClientEnv -> ClientM a -> IO (Either ClientError a)
testClientOrError = flip runClientM
#else
testClientOrError :: ClientEnv -> ClientM a -> IO (Either ServantError a)
testClientOrError = flip runClientM
#endif

instance IsTest (ClientM ()) where
  type Arg1 (ClientM ()) = ()
  type Arg2 (ClientM ()) = ClientEnv
  runTest func = runTest (\() -> func)

instance IsTest (outerArgs -> ClientM ()) where
  type Arg1 (outerArgs -> ClientM ()) = outerArgs
  type Arg2 (outerArgs -> ClientM ()) = ClientEnv
  runTest func = runTest (\outerArgs clientEnv -> testClient clientEnv (func outerArgs))
