{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.Servant.ExampleWithContext where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Servant
import Servant.Client

exampleAPI :: Proxy ExampleAPI
exampleAPI = Proxy

newtype User = User String

type ExampleAPI =
  BasicAuth "test" User
    :> ( "get" :> Get '[JSON] Int
           :<|> "add" :> ReqBody '[JSON] Int :> Post '[JSON] NoContent
       )

exampleServer :: TVar Int -> Server ExampleAPI
exampleServer var = const $ serveGet :<|> serveAdd
  where
    serveGet :: Handler Int
    serveGet = liftIO $ readTVarIO var
    serveAdd :: Int -> Handler NoContent
    serveAdd i = do
      liftIO $ atomically $ modifyTVar var (+ i)
      pure NoContent

exampleApplication :: TVar Int -> Application
exampleApplication var = serveWithContext exampleAPI exampleContext (exampleServer var)

exampleContext :: Context '[BasicAuthCheck User]
exampleContext = checkBasicAuth :. EmptyContext

checkBasicAuth :: BasicAuthCheck User
checkBasicAuth = BasicAuthCheck $ \basicAuthData ->
  let username = basicAuthUsername basicAuthData
      password = basicAuthPassword basicAuthData
   in pure $ if username == "foo" && password == "bar" then Authorized (User "foo") else Unauthorized

clientGetCorrectCredentials :: ClientM Int
clientAddCorrectCredentials :: Int -> ClientM NoContent
(clientGetCorrectCredentials :<|> clientAddCorrectCredentials) = client exampleAPI BasicAuthData {basicAuthUsername = "foo", basicAuthPassword = "bar"}

clientGetWrongCredentials :: ClientM Int
clientAddWrongCredentials :: Int -> ClientM NoContent
(clientGetWrongCredentials :<|> clientAddWrongCredentials) = client exampleAPI BasicAuthData {basicAuthUsername = "wrong", basicAuthPassword = "wrong"}
