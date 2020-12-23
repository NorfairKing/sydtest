{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Servant
  ( servantSpec,
    servantSpecWithSetupFunc,
    clientEnvSetupFunc,
    testClient,
    testClientOrError,
  )
where

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
servantSpecWithSetupFunc :: forall api a. HasServer api '[] => Servant.Proxy api -> SetupFunc a (ServerT api Handler) -> ServantSpec -> SpecWith a
servantSpecWithSetupFunc py serverSetupFunc =
  beforeAll (newManager defaultManagerSettings)
    . setupAroundWith' (\man -> serverSetupFunc `connectSetupFunc` clientEnvSetupFunc py man)

clientEnvSetupFunc :: forall api. HasServer api '[] => Servant.Proxy api -> HTTP.Manager -> SetupFunc (ServerT api Handler) ClientEnv
clientEnvSetupFunc py man = wrapSetupFunc $ \server -> do
  let application = serve py server
  p <- unwrapSetupFunc applicationSetupFunc application
  pure $ mkClientEnv man (BaseUrl Http "127.0.0.1" p "")

testClient :: ClientEnv -> ClientM a -> IO a
testClient cenv func = do
  errOrRes <- testClientOrError cenv func
  case errOrRes of
    Left err -> expectationFailure $ show err
    Right r -> pure r

testClientOrError :: ClientEnv -> ClientM a -> IO (Either ClientError a)
testClientOrError = flip runClientM
