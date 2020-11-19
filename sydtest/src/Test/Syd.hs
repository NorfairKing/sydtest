{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd where

import Control.Concurrent.Process.StoredMVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception
import Data.Aeson as JSON
import Data.ByteString
import qualified Data.ByteString.Lazy as LB
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Foreign.StablePtr
import GHC.Generics (Generic)
import System.Exit
import System.IO (hFlush)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process
import System.Posix.SharedMem

type Test = IO ()

data TestRunResult = TestRunResult {testRunResultStatus :: TestStatus}
  deriving (Show)

data TestStatus = TestPassed | TestFailed
  deriving (Show, Generic)

instance FromJSON TestStatus

instance ToJSON TestStatus

runTest :: Test -> IO TestRunResult
runTest func = do
  testRunResultStatus <-
    runInProcess $
      (func >>= (evaluate . (`seq` TestPassed)))
        `catches` [ Handler $ \(_ :: ErrorCall) -> pure TestFailed,
                    Handler $ \(_ :: ExitCode) -> pure TestFailed,
                    Handler $ \(_ :: RecConError) -> pure TestFailed,
                    Handler $ \(_ :: RecSelError) -> pure TestFailed,
                    Handler $ \(_ :: RecUpdError) -> pure TestFailed,
                    Handler $ \(_ :: PatternMatchFail) -> pure TestFailed
                  ]
  pure TestRunResult {..}

runInProcess :: (FromJSON a, ToJSON a) => IO a -> IO a
runInProcess func = do
  -- FIXME: Forking a process is expensive and potentially not needed for pure code.
  uuid <- UUID.nextRandom
  print uuid
  (sharedMemOutsideFd, sharedMemInsideFd) <- createPipe
  -- sharedMemOutsideFd <- shmOpen (UUID.toString uuid) (ShmOpenFlags {shmReadWrite = True, shmCreate = True, shmExclusive = False, shmTrunc = True}) ownerModes
  sharedMemOutsideHandle <- fdToHandle sharedMemOutsideFd
  writeVar <- newEmptyMVar
  readVar <- newEmptyMVar
  print "Creating test process"
  testProcess <- forkProcess $ do
    sharedMemInsideHandle <- fdToHandle sharedMemInsideFd
    print "starting test"
    status <- func
    print "test done"
    let statusBs = LB.toStrict $ JSON.encode status
    print statusBs
    hPut sharedMemInsideHandle statusBs
    hFlush sharedMemInsideHandle
    print "written"
    putMVar writeVar ()
    print "test waiting to be read"
    -- Wait for the status to be read
    takeMVar readVar
    print "test process done"
  print "outer waiting to read"
  () <- takeMVar writeVar
  statusBs <- LB.fromStrict <$> hGetContents sharedMemOutsideHandle
  print statusBs
  print "read"
  putMVar readVar ()
  case JSON.eitherDecode statusBs of
    Left err -> die err
    Right res -> do
      putMVar readVar ()
      print "read"
      ps <- getProcessStatus True False testProcess
      print ps
      pure res
