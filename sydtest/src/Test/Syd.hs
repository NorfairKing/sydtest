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
  sharedMemInsideHandle <- fdToHandle sharedMemInsideFd
  testProcess <- forkProcess $ do
    -- Be nice while testing so that the computer cannot lock up.
    nice 19
    -- Don't input or output anything
    newStdin <- createFile "/dev/null" ownerModes
    dupTo newStdin stdInput
    newStdout <- createFile "/dev/null" ownerModes
    dupTo newStdout stdOutput
    newStderr <- createFile "/dev/null" ownerModes
    dupTo newStderr stdError
    -- Actually run the function
    status <- func
    -- Encode the output
    let statusBs = LB.toStrict $ JSON.encode status
    -- Write it to the pipe
    hPut sharedMemInsideHandle statusBs
    hFlush sharedMemInsideHandle
    hClose sharedMemInsideHandle
  -- Wait for the testing process to finish
  _ <- getProcessStatus True False testProcess
  -- Read its result from the pipe
  statusBs <- LB.fromStrict <$> hGetSome sharedMemOutsideHandle 1024
  case JSON.eitherDecode statusBs of
    Left err -> die err -- This cannot happen if the result was fewer than 1024 bytes in size
    Right res -> pure res
