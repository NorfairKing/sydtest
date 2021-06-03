{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- TODO possibly supply a variant of 'redisSpec' that uses a different database scope per test
-- so that the tests can still happen in parallel:
-- see https://hackage.haskell.org/package/hedis-0.14.2/docs/Database-Redis.html#v:select
-- and connectDatabase:
-- https://hackage.haskell.org/package/hedis-0.14.2/docs/Database-Redis.html#t:ConnectInfo
module Test.Syd.Redis
  ( redisSpec,
    redisConnectionSetupFunc,
    checkedConnectSetupFunc,
    RedisServerHandle (..),
    redisServerSpec,
    cleanRedisServerState,
    redisServerSetupFunc,
    redisServerSetupFunc',
  )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Redis as Redis
import Network.Socket
import Network.Socket.Free
import qualified Network.Socket.Wait as Socket
import Path
import Path.IO
import System.Process.Typed
import Test.Syd
import Test.Syd.Path
import Test.Syd.Process.Typed

-- | A handle to a child process that is a Redis server.
data RedisServerHandle = RedisServerHandle
  { redisServerHandleProcessHandle :: !(Process () () ()),
    redisServerHandlePort :: !PortNumber
  }

-- | Run a redis server around a group of test and provide a clean connection to every test
--
-- Example usage:
--
-- >  redisSpec $ do
-- >    it "sets up and tears down a redis connection nicely" $ \conn -> do
-- >      runRedis conn $ do
-- >        errOrStatus <- Redis.set "hello" "world"
-- >        liftIO $ case errOrStatus of
-- >          Left err -> expectationFailure $ show err
-- >          Right status -> status `shouldBe` Ok
-- >        errOrReply <- Redis.get "hello"
-- >        liftIO $ case errOrReply of
-- >          Left err -> expectationFailure $ show err
-- >          Right val -> val `shouldBe` Just "world"
--
-- This function just combines 'redisServerSpec' with 'setupAroundWith' redisConnectionSetupFunc'.
redisSpec :: TestDefM (RedisServerHandle ': outers) Redis.Connection result -> TestDefM outers inner result
redisSpec = redisServerSpec . setupAroundWith' (\serverHandle _ -> redisConnectionSetupFunc serverHandle)

-- | Set up a clean redis connection given a handle to the redis server.
--
-- This function cleans the state using `flushall`.
redisConnectionSetupFunc :: RedisServerHandle -> SetupFunc Redis.Connection
redisConnectionSetupFunc RedisServerHandle {..} = do
  let connInfo = Redis.defaultConnectInfo {connectPort = PortNumber redisServerHandlePort}
  conn <- checkedConnectSetupFunc connInfo
  SetupFunc $ \func -> do
    cleanRedisServerState conn
    func conn

-- | A 'SetupFunc' that 'bracket's 'checkedConnect' and 'disconnect'.
checkedConnectSetupFunc :: Redis.ConnectInfo -> SetupFunc Redis.Connection
checkedConnectSetupFunc connInfo = bracketSetupFunc (checkedConnect connInfo) disconnect

-- | Run a redis server around a group of tests.
redisServerSpec :: TestDefM (RedisServerHandle ': outers) inner result -> TestDefM outers inner result
redisServerSpec = setupAroundAll redisServerSetupFunc . sequential -- Must run sequentially because state is shared.

-- | Clean the redis server's state.
cleanRedisServerState :: Connection -> IO ()
cleanRedisServerState conn = do
  errOrStatus <- runRedis conn Redis.flushall -- Clean state
  case errOrStatus of
    Left err -> expectationFailure $ "Something went wrong while trying to clean the state before the test starts: " <> show err
    Right s -> s `shouldBe` Ok

-- | Setup func for running a Redis server
--
-- This function uses a temporary directory (using 'tempDirSetupFunc') for any state.
redisServerSetupFunc :: SetupFunc RedisServerHandle
redisServerSetupFunc = do
  td <- tempDirSetupFunc "sydtest-hedis"
  redisServerSetupFunc' td

-- | Setup func for running a Redis server in a given directory
redisServerSetupFunc' :: Path Abs Dir -> SetupFunc RedisServerHandle
redisServerSetupFunc' td = do
  pidFile <- resolveFile td "redis.pid"
  logFile <- resolveFile td "redis.log"
  portInt <- liftIO $ do
    (portInt, _socket) <- openFreePort
    close _socket
    pure portInt
  let pn = fromIntegral portInt -- (hopefully) safe because it came from 'getFreePort'.
  let configFileContents =
        T.pack $
          unlines
            [ unwords ["port", show (fromIntegral pn :: Int)],
              unwords ["pidfile", fromAbsFile pidFile],
              unwords ["always-show-logo", "no"], -- No need to see the logo.
              unwords ["logfile", fromAbsFile logFile]
            ]
  configFile <- tempBinaryFileWithContentsSetupFunc "config-file" (TE.encodeUtf8 configFileContents)
  let pc =
        setWorkingDir (fromAbsDir td) $
          setStdout inherit $
            setStderr inherit $
              proc
                "redis-server"
                [fromAbsFile configFile]
  ph <- typedProcessSetupFunc pc
  liftIO $ Socket.wait "127.0.0.1" portInt
  pure $
    RedisServerHandle
      { redisServerHandleProcessHandle = ph,
        redisServerHandlePort = pn
      }
