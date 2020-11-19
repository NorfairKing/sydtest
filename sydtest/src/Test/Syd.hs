{-# LANGUAGE DeriveFunctor #-}
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
import Data.Foldable
import Data.Traversable
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

type SpecForest a = [SpecTree a]

data SpecTree a
  = DescribeNode String (SpecForest a) -- A description
  | SpecifyNode String a -- A test with its description
  deriving (Show, Functor)

instance Foldable SpecTree where
  foldMap f = \case
    DescribeNode _ sts -> foldMap (foldMap f) sts
    SpecifyNode _ a -> f a

instance Traversable SpecTree where
  traverse func = \case
    DescribeNode s sts -> DescribeNode s <$> traverse (traverse func) sts
    SpecifyNode s a -> SpecifyNode s <$> func a

runSpecForest :: SpecForest Test -> IO (SpecForest TestRunResult)
runSpecForest = traverse (traverse runTest)

runSpecTree :: SpecTree Test -> IO (SpecTree TestRunResult)
runSpecTree = traverse runTest

runTest :: Test -> IO TestRunResult
runTest func = do
  testRunResultStatus <-
    runInSilencedNiceProcess $
      (func >>= (evaluate . (`seq` TestPassed)))
        `catches` [ Handler $ \(_ :: ErrorCall) -> pure TestFailed,
                    Handler $ \(_ :: ExitCode) -> pure TestFailed,
                    Handler $ \(_ :: RecConError) -> pure TestFailed,
                    Handler $ \(_ :: RecSelError) -> pure TestFailed,
                    Handler $ \(_ :: RecUpdError) -> pure TestFailed,
                    Handler $ \(_ :: PatternMatchFail) -> pure TestFailed
                  ]
  pure TestRunResult {..}

runInSilencedNiceProcess :: (FromJSON a, ToJSON a) => IO a -> IO a
runInSilencedNiceProcess func = do
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
