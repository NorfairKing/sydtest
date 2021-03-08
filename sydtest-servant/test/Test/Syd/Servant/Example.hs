{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.Servant.Example where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Servant
import Servant.Client

exampleAPI :: Proxy ExampleAPI
exampleAPI = Proxy

type ExampleAPI =
  "get" :> Get '[JSON] Int
    :<|> "add" :> ReqBody '[JSON] Int :> Post '[JSON] NoContent

exampleServer :: TVar Int -> Server ExampleAPI
exampleServer var = serveGet :<|> serveAdd
  where
    serveGet :: Handler Int
    serveGet = liftIO $ readTVarIO var
    serveAdd :: Int -> Handler NoContent
    serveAdd i = do
      liftIO $ atomically $ modifyTVar var (+ i)
      pure NoContent

exampleApplication :: TVar Int -> Application
exampleApplication var = serve exampleAPI (exampleServer var)

clientGet :: ClientM Int
clientAdd :: Int -> ClientM NoContent
clientGet :<|> clientAdd = client exampleAPI
