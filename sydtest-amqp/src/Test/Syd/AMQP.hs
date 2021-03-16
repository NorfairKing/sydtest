{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.AMQP where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Network.AMQP as AMQP
import Network.Socket
import Network.Socket.Free
import qualified Network.Socket.Wait as Socket
import Path
import Path.IO
import System.Environment
import System.Process
import Test.Syd

data RabbitMQHandle = RabbitMQHandle
  { rabbitMQHandleProcessHandle :: !ProcessHandle,
    rabbitMQHandlePort :: !PortNumber,
    rabbitMQHandleDistributionPort :: !PortNumber
  }

amqpSpec :: TestDefM (RabbitMQHandle ': outers) AMQP.Connection result -> TestDefM outers () result
amqpSpec = rabbitMQSpec . setupAroundWith' amqpConnectionSetupFunc

amqpConnectionSetupFunc :: RabbitMQHandle -> SetupFunc () Connection
amqpConnectionSetupFunc RabbitMQHandle {..} =
  makeSimpleSetupFunc $ \func -> do
    let opts = defaultConnectionOpts {coServers = [("localhost", rabbitMQHandlePort)]}
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
  let cp = (proc "rabbitmq-server" []) {env = Just e}
  ph <-
    makeSimpleSetupFunc
      ( \func -> withCreateProcess cp $ \inh outh errh ph -> do
          Socket.wait "127.0.0.1" portInt
          putStrLn "RabbitMQ ready for testing!"
          r <- func ph
          cleanupProcess (inh, outh, errh, ph)
          pure r
      )
  let pn = fromIntegral portInt -- (hopefully) safe because it came from 'getFreePort'.
  let dpn = fromIntegral distPortInt -- (hopefully) safe because it came from 'getFreePort'.
  pure $
    RabbitMQHandle
      { rabbitMQHandleProcessHandle = ph,
        rabbitMQHandlePort = pn,
        rabbitMQHandleDistributionPort = dpn
      }
