module Test.Syd.AMQP where

import Control.Concurrent.Async
import Network.AMQP
import Path
import Path.IO
import System.Environment
import System.Process
import Test.Syd

-- rabbitMQSpec

runRabbitMQServer :: IO ()
runRabbitMQServer = withSystemTempDir "sydtest-amqp" $ \td -> do
  pidFile <- resolveFile td "rabbitmq.pid"
  configFile <- resolveFile td "rabbitmq.conf"
  mnesiaDir <- resolveDir td "mnesia"
  logDir <- resolveDir td "log"
  oldEnv <- getEnvironment
  let e =
        [ ("RABBITMQ_PID_FILE", fromAbsFile pidFile),
          ("RABBITMQ_CONFIG_FILE", fromAbsFile configFile),
          ("RABBITMQ_MNESIA_DIR", fromAbsDir mnesiaDir),
          ("RABBITMQ_MNESIA_BASE", fromAbsDir mnesiaDir), -- Just to be sure
          ("RABBITMQ_LOG_BASE", fromAbsDir logDir),
          ("RABBITMQ_LOGS", fromAbsDir logDir) -- Just to be sure
        ]
          ++ oldEnv
  let cp = (proc "rabbitmq-server" []) {env = Just e}
  withCreateProcess cp $ \_ _ _ ph ->
    waitForProcess ph >>= print
