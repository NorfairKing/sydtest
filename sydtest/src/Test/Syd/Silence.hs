module Test.Syd.Silence where

import Data.Aeson as JSON
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import System.Exit
import System.IO (hClose, hFlush)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process

runInSilencedNiceProcess :: (FromJSON a, ToJSON a) => IO a -> IO a
runInSilencedNiceProcess func = do
  (sharedMemOutsideFd, sharedMemInsideFd) <- createPipe
  sharedMemOutsideHandle <- fdToHandle sharedMemOutsideFd
  sharedMemInsideHandle <- fdToHandle sharedMemInsideFd
  testProcess <- forkProcess $ do
    -- Be nice while testing so that the computer cannot lock up.
    nice 19
    -- Don't input or output anything
    newStdin <- createFile "/dev/null" ownerModes
    _ <- dupTo newStdin stdInput
    newStdout <- createFile "/dev/null" ownerModes
    _ <- dupTo newStdout stdOutput
    newStderr <- createFile "/dev/null" ownerModes
    _ <- dupTo newStderr stdError
    -- Actually run the function
    status <- func
    -- Encode the output
    let statusBs = LB.toStrict $ JSON.encode status
    -- Write it to the pipe
    SB.hPut sharedMemInsideHandle statusBs
    hFlush sharedMemInsideHandle
    hClose sharedMemInsideHandle
  -- Wait for the testing process to finish
  _ <- getProcessStatus True False testProcess
  -- Read its result from the pipe
  statusBs <- LB.fromStrict <$> SB.hGetSome sharedMemOutsideHandle 10240 -- FIXME figure out a way to get rid of this magic constant
  case JSON.eitherDecode statusBs of
    Left err -> die err -- This cannot happen if the result was fewer than 1024 bytes in size
    Right res -> pure res
