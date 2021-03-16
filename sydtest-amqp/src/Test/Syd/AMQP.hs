{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.AMQP where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.AMQP as AMQP
import Network.Socket
import Network.Socket.Free
import qualified Network.Socket.Wait as Socket
import Path
import Path.IO
import System.Environment (getEnvironment)
import System.Process.Typed
import Test.Syd

data RabbitMQHandle = RabbitMQHandle
  { rabbitMQHandleProcessHandle :: !(Process () () ()),
    rabbitMQHandlePort :: !PortNumber,
    rabbitMQHandleDistributionPort :: !PortNumber
  }

amqpSpec :: TestDefM (RabbitMQHandle ': outers) AMQP.Connection result -> TestDefM outers () result
amqpSpec = rabbitMQSpec . setupAroundWith' amqpConnectionSetupFunc

amqpConnectionSetupFunc :: RabbitMQHandle -> SetupFunc () Connection
amqpConnectionSetupFunc h = do
  liftIO $ cleanRabbitMQState h
  makeSimpleSetupFunc $ \func -> do
    let opts = defaultConnectionOpts {coServers = [("localhost", rabbitMQHandlePort h)]}
    let acquire = openConnection'' opts
    let release = closeConnection
    let use = func
    bracket acquire release use

-- | Sets up a rabbitmq server, once, for the given group of tests.
--
-- Note that this does not clean up anything between tests. See 'amqpSpec' instead.
rabbitMQSpec :: TestDefM (RabbitMQHandle ': outers) inner result -> TestDefM outers inner result
rabbitMQSpec = setupAroundAll rabbitMQServerSetupFunc

rabbitMQServerSetupFunc :: SetupFunc () RabbitMQHandle
rabbitMQServerSetupFunc = do
  td <- makeSimpleSetupFunc $ withSystemTempDir "sydtest-amqp"
  unwrapSetupFunc rabbitMQServerSetupFunc' td

rabbitMQServerSetupFunc' :: SetupFunc (Path Abs Dir) RabbitMQHandle
rabbitMQServerSetupFunc' = wrapSetupFunc $ \td -> do
  pidFile <- resolveFile td "rabbitmq.pid"
  configFile <- resolveFile td "rabbitmq.conf"
  mnesiaDir <- resolveDir td "mnesia"
  logDir <- resolveDir td "log"
  oldEnv <- liftIO getEnvironment -- We may not want to leak all of this in?
  portInt <- liftIO getFreePort
  distPortInt <- liftIO getFreePort
  liftIO $ putStrLn $ unwords ["Starting RabbitMQ Server on port", show portInt]
  let e =
        [ ("RABBITMQ_PID_FILE", fromAbsFile pidFile),
          ("RABBITMQ_CONFIG_FILE", fromAbsFile configFile),
          ("RABBITMQ_MNESIA_DIR", fromAbsDir mnesiaDir),
          ("RABBITMQ_MNESIA_BASE", fromAbsDir mnesiaDir), -- Just to be sure
          ("RABBITMQ_LOG_BASE", fromAbsDir logDir),
          ("RABBITMQ_LOGS", fromAbsDir logDir), -- Just to be sure
          ("RABBITMQ_NODE_PORT", show portInt),
          ("RABBITMQ_DIST_PORT", show distPortInt)
        ]
          ++ oldEnv
  let pc = setStdout inherit $ setStderr inherit $ setEnv e $ (proc "rabbitmq-server" [])
  ph <-
    makeSimpleSetupFunc
      ( \func -> withProcessWait pc $ \ph -> do
          Socket.wait "127.0.0.1" portInt
          putStrLn "RabbitMQ ready for testing!"
          func ph
      )
  let pn = fromIntegral portInt -- (hopefully) safe because it came from 'getFreePort'.
  let dpn = fromIntegral distPortInt -- (hopefully) safe because it came from 'getFreePort'.
  pure $
    RabbitMQHandle
      { rabbitMQHandleProcessHandle = ph,
        rabbitMQHandlePort = pn,
        rabbitMQHandleDistributionPort = dpn
      }

cleanRabbitMQState :: RabbitMQHandle -> IO ()
cleanRabbitMQState RabbitMQHandle {..} = do
  lb <- readProcessStdout_ $ shell "rabbitmqctl list_queues --formatter json"
  case JSON.eitherDecode lb of
    Left err -> fail err
    Right r -> print (r :: ListQueuesOutput)

newtype ListQueuesOutput = ListQueuesOutput [QueueOutput]
  deriving (Show, Eq, Generic)

instance FromJSON ListQueuesOutput where
  parseJSON v = ListQueuesOutput <$> parseJSON v

data QueueOutput = QueueOutput
  { queueOutputName :: Text,
    queueOutputMessages :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON QueueOutput where
  parseJSON = withObject "QueueOutput" $ \o -> QueueOutput <$> o .: "name" <*> o .: "messages"
