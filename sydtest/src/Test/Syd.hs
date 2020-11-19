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
  -- FIXME: Forking a process is expensive and potentially not needed for pure code.
  uuid <- UUID.nextRandom
  sharedMemOutsideFd <- shmOpen (UUID.toString uuid) (ShmOpenFlags {shmReadWrite = False, shmCreate = True, shmExclusive = True, shmTrunc = True}) ownerReadMode
  sharedMemOutsideHandle <- fdToHandle sharedMemOutsideFd
  writeVar <- newEmptyMVar
  readVar <- newEmptyMVar
  testProcess <- forkProcess $ do
    sharedMemInsideFd <- shmOpen (UUID.toString uuid) (ShmOpenFlags {shmReadWrite = True, shmCreate = False, shmExclusive = False, shmTrunc = False}) ownerWriteMode
    sharedMemInsideHandle <- fdToHandle sharedMemInsideFd
    status <-
      (func >>= (evaluate . (`seq` TestPassed)))
        `catches` [ Handler $ \(_ :: ErrorCall) -> pure TestFailed,
                    Handler $ \(_ :: ExitCode) -> pure TestFailed,
                    Handler $ \(_ :: RecConError) -> pure TestFailed,
                    Handler $ \(_ :: RecSelError) -> pure TestFailed,
                    Handler $ \(_ :: RecUpdError) -> pure TestFailed,
                    Handler $ \(_ :: PatternMatchFail) -> pure TestFailed
                  ]
    LB.hPut sharedMemInsideHandle $ JSON.encode status
    putMVar writeVar ()
    -- Wait for the status to be read
    takeMVar readVar
  () <- takeMVar readVar
  statusBs <- LB.hGetContents sharedMemOutsideHandle
  case JSON.eitherDecode statusBs of
    Left err -> die err
    Right testRunResultStatus -> do
      putMVar readVar ()
      print "read"
      ps <- getProcessStatus True False testProcess
      print ps
      pure TestRunResult {..}
