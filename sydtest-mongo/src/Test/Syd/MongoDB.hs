{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.MongoDB where

import Data.Text (Text)
import Data.Yaml as Yaml
import Network.Socket as Socket
import Network.Socket.Free
import qualified Network.Socket.Wait as Socket
import Path
import Path.IO
import System.Process
import Test.Syd
import Test.Syd.Path
import Test.Syd.Process

data MongoServerHandle = MongoServerHandle
  { mongoServerHandleProcessHandle :: !ProcessHandle,
    mongoServerHandlePort :: !PortNumber
  }

mongoServerSpec :: TestDefM (MongoServerHandle ': outers) inner result -> TestDefM outers inner result
mongoServerSpec = setupAroundAll mongoServerSetupFunc . sequential -- Must run sequentially because state is shared.

mongoServerSetupFunc :: SetupFunc () MongoServerHandle
mongoServerSetupFunc = do
  td <- tempDirSetupFunc "sydtest-hedis"
  unwrapSetupFunc mongoServerSetupFunc' td

mongoServerSetupFunc' :: SetupFunc (Path Abs Dir) MongoServerHandle
mongoServerSetupFunc' = wrapSetupFunc $ \td -> do
  pidFile <- resolveFile td "mongo.pid"
  logFile <- resolveFile td "mongo.log"
  dataDir <- resolveDir td "data"
  ensureDir dataDir
  portInt <- liftIO $ do
    (portInt, _socket) <- openFreePort
    Socket.close _socket
    pure portInt
  let pn = fromIntegral portInt -- (hopefully) safe because it came from 'getFreePort'.
  let configFileContents =
        Yaml.encode $
          object
            [ "systemLog"
                .= object
                  [ "destination" .= ("file" :: Text),
                    "path" .= fromAbsFile logFile
                  ],
              "net"
                .= object
                  [ "port" .= portInt
                  ],
              "processManagement"
                .= object
                  ["fork" .= False, "pidFilePath" .= fromAbsFile pidFile],
              -- It would be nice to use the in-memory storage engine
              -- but that's only available in mongodb enterprise >=3.2.
              "storage"
                .= object
                  [ "dbPath" .= fromAbsDir dataDir
                  ]
            ]
  configFile <- tempBinaryFileWithContentsSetupFunc "config-file" configFileContents
  let pc =
        ( proc
            "mongod"
            ["--config", fromAbsFile configFile]
        )
          { cwd = Just $ fromAbsDir td
          }
  (_, _, _, ph) <- processSetupFunc pc
  liftIO $ Socket.wait "127.0.0.1" portInt
  pure $
    MongoServerHandle
      { mongoServerHandleProcessHandle = ph,
        mongoServerHandlePort = pn
      }
