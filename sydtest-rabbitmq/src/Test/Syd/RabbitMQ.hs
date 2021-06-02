{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.RabbitMQ
  ( RabbitMQHandle (..),
    rabbitMQSpec,
    rabbitMQServerSetupFunc,
    rabbitMQServerSetupFunc',
    cleanRabbitMQStateBeforeEach,
    cleanRabbitMQState,
  )
where

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
import System.IO
import System.Process.Typed
import Test.Syd
import Test.Syd.Path
import Test.Syd.Process.Typed

data RabbitMQHandle = RabbitMQHandle
  { rabbitMQHandleProcessHandle :: !(Process () Handle Handle),
    rabbitMQHandlePort :: !PortNumber
  }

-- | Run a rabbitmq server around a group of tests.
--
-- Example usage:
--
-- > rabbitMQSpec $ do
-- >   describe "rabbitMQSpec" $
-- >     it "can run the server and clean up nicely" $ do
-- >       pure () :: IO ()
--
-- (You will probably want to use `sydtest-amqp` or `sydtest-amqp-client`
-- instead of directly using this package.)
--
-- This function also cleans up the rabbitmq state before every test.
-- Consequently, it also uses @modifyMaxSuccess (`div` 20)@ to decrease the
-- number of property tests that will be run, because the state cleaning takes
-- a long time.
rabbitMQSpec :: TestDefM (RabbitMQHandle ': outers) inner result -> TestDefM outers inner result
rabbitMQSpec =
  setupAroundAll rabbitMQServerSetupFunc
    . sequential -- Must run sequentially because state is shared.
    . cleanRabbitMQStateBeforeEach
    . modifyMaxSuccess (`div` 20)

-- | Set up a rabbitmq server in a temporary directory.
rabbitMQServerSetupFunc :: SetupFunc RabbitMQHandle
rabbitMQServerSetupFunc = do
  td <- tempDirSetupFunc "sydtest-amqp"
  rabbitMQServerSetupFunc' td

-- | Set up a rabbitmq server in the given directory
rabbitMQServerSetupFunc' :: Path Abs Dir -> SetupFunc RabbitMQHandle
rabbitMQServerSetupFunc' td = do
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
  let pc = setWorkingDir (fromAbsDir td) $ setStdout createPipe $ setStderr createPipe $ setEnv e $ proc "rabbitmq-server" []
  ph <- typedProcessSetupFunc pc
  liftIO $ Socket.wait "127.0.0.1" portInt
  let pn = fromIntegral portInt -- (hopefully) safe because it came from 'getFreePort'.
  pure $
    RabbitMQHandle
      { rabbitMQHandleProcessHandle = ph,
        rabbitMQHandlePort = pn
      }

cleanRabbitMQStateBeforeEach :: TestDefM (RabbitMQHandle ': outers) inner result -> TestDefM (RabbitMQHandle ': outers) inner result
cleanRabbitMQStateBeforeEach =
  beforeWith' $ \handle inner -> do
    cleanRabbitMQState handle
    pure inner

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
