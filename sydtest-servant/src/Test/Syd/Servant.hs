{-# LANGUAGE CPP #-}
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
