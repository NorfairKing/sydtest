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
import Foreign.StablePtr
import GHC.Generics (Generic)
import System.Exit
import System.IO (hClose, hFlush)
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
  (sharedMemOutsideFd, sharedMemInsideFd) <- createPipe
  sharedMemOutsideHandle <- fdToHandle sharedMemOutsideFd
  doneVar <- newEmptyMVar
  testProcess <- forkProcess $ do
    nice 19 -- Be nice
    sharedMemInsideHandle <- fdToHandle sharedMemInsideFd
    status <- func
    let statusBs = LB.toStrict $ JSON.encode status
    hPut sharedMemInsideHandle statusBs
    hFlush sharedMemInsideHandle
    hClose sharedMemInsideHandle
    -- Wait for the status to be read
    putMVar doneVar ()
  () <- takeMVar doneVar
  statusBs <- LB.fromStrict <$> hGetSome sharedMemOutsideHandle 1024
  case JSON.eitherDecode statusBs of
    Left err -> die err
    Right res -> do
      -- Wait to make sure the process is finished.
      _ <- getProcessStatus True False testProcess
      pure res
