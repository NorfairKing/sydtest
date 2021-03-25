{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.RabbitMQ where

import Control.Monad
import Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Socket
import Network.Socket.Free
import qualified Network.Socket.Wait as Socket
import Path
import Path.IO
import System.Environment (getEnvironment)
import System.Exit
import System.Process.Typed
import Test.Syd
import Test.Syd.Path
import Test.Syd.Process.Typed

data RabbitMQHandle = RabbitMQHandle
  { rabbitMQHandleProcessHandle :: !(Process () () ()),
    rabbitMQHandlePort :: !PortNumber
  }

-- Note that this does not clean up anything between tests. See 'amqpSpec' instead.
rabbitMQSpec :: TestDefM (RabbitMQHandle ': outers) inner result -> TestDefM outers inner result
rabbitMQSpec = setupAroundAll rabbitMQServerSetupFunc . sequential -- Must run sequentially because state is shared.

rabbitMQServerSetupFunc :: SetupFunc () RabbitMQHandle
rabbitMQServerSetupFunc = do
  td <- tempDirSetupFunc "sydtest-amqp"
  unwrapSetupFunc rabbitMQServerSetupFunc' td

rabbitMQServerSetupFunc' :: SetupFunc (Path Abs Dir) RabbitMQHandle
rabbitMQServerSetupFunc' = wrapSetupFunc $ \td -> do
  pidFile <- resolveFile td "rabbitmq.pid"
  configFile <- resolveFile td "rabbitmq.conf"
  mnesiaDir <- resolveDir td "mnesia"
  schemaDir <- resolveDir td "schema"
  pluginsDir <- resolveDir td "plugins"
  pluginsExpandDir <- resolveDir td "plugins-expand"
  generatedConfigDir <- resolveDir td "generated-config"
  logDir <- resolveDir td "log"
  ensureDir logDir
  let getFreePort_ = liftIO $ do
        (portInt, _socket) <- openFreePort
        close _socket
        pure portInt
  portInt <- liftIO getFreePort_
  distPortInt <- liftIO getFreePort_
  oldEnv <- liftIO getEnvironment -- We may not want to leak all of this in?
  let e =
        [ ("RABBITMQ_BASE", fromAbsDir td),
          ("RABBITMQ_PID_FILE", fromAbsFile pidFile),
          ("RABBITMQ_CONFIG_FILE", fromAbsFile configFile),
          ("RABBITMQ_MNESIA_DIR", fromAbsDir mnesiaDir),
          ("RABBITMQ_MNESIA_BASE", fromAbsDir mnesiaDir), -- Just to be sure
          ("RABBITMQ_SCHEMA_DIR", fromAbsDir schemaDir),
          ("RABBITMQ_PLUGINS_DIR", fromAbsDir pluginsDir),
          ("RABBITMQ_PLUGINS_EXPAND_DIR", fromAbsDir pluginsExpandDir),
          ("RABBITMQ_GENERATED_CONFIG_DIR", fromAbsDir generatedConfigDir),
          ("RABBITMQ_LOG_BASE", fromAbsDir logDir),
          ("RABBITMQ_LOGS", fromAbsDir logDir), -- Just to be sure
          ("RABBITMQ_NODE_PORT", show portInt),
          ("RABBITMQ_DIST_PORT", show distPortInt)
        ]
          ++ oldEnv
  let pc = setWorkingDir (fromAbsDir td) $ setStdout inherit $ setStderr inherit $ setEnv e $ proc "rabbitmq-server" []
  ph <- typedProcessSetupFunc pc
  liftIO $ Socket.wait "127.0.0.1" portInt
  let pn = fromIntegral portInt -- (hopefully) safe because it came from 'getFreePort'.
  pure $
    RabbitMQHandle
      { rabbitMQHandleProcessHandle = ph,
        rabbitMQHandlePort = pn
      }

-- FIXME: I'd prefer if there was a less-external way to do this, but oh well :s
cleanRabbitMQState :: RabbitMQHandle -> IO ()
cleanRabbitMQState RabbitMQHandle {..} = do
  oldEnv <- liftIO getEnvironment -- We may not want to leak all of this in?
  let e =
        ("RABBITMQ_NODE_PORT", show (fromIntegral rabbitMQHandlePort :: Int)) :
        oldEnv

  (_ec, _output) <- readProcessInterleaved $ setEnv e $ shell "rabbitmqctl close_all_connections cleanup"
  case _ec of
    ExitFailure i -> fail $ unlines ["Something went wrong while trying to close connections", "exit code: " <> show i, show _output]
    ExitSuccess -> pure ()
  lb <- readProcessStdout_ $ setEnv e $ shell "rabbitmqctl list_queues --formatter json"
  case JSON.eitherDecode lb of
    Left err -> fail err
    Right (ListQueuesOutput queues) -> forM_ queues $ \queue -> do
      let queueName = T.unpack (queueOutputName queue)
      (_ec, _output) <- readProcessInterleaved $ setEnv e $ shell $ "rabbitmqctl purge_queue " <> queueName
      case _ec of
        ExitFailure i -> fail $ unlines ["Something went wrong while trying to purge queue " <> queueName, "exit code: " <> show i, show _output]
        ExitSuccess -> pure ()
      (_ec, _output) <- readProcessInterleaved $ setEnv e $ shell $ "rabbitmqctl delete_queue " <> queueName
      case _ec of
        ExitFailure i -> fail $ unlines ["Something went wrong while trying to delete queue " <> queueName, "exit code: " <> show i, show _output]
        ExitSuccess -> pure ()

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
