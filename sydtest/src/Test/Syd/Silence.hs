module Test.Syd.Silence where

import Data.Compact
import Data.Compact.Serialize
import Data.Typeable
import System.Exit
import System.IO (hClose, hFlush)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process

runInSilencedNiceProcess :: Typeable a => IO a -> IO a
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
    result <- func
    compactRegion <- compact result
    hPutCompact sharedMemInsideHandle compactRegion
    hFlush sharedMemInsideHandle
    hClose sharedMemInsideHandle
  -- Wait for the testing process to finish
  _ <- getProcessStatus True False testProcess
  -- Read its result from the pipe
  errOrResult <- hUnsafeGetCompact sharedMemOutsideHandle
  case errOrResult of
    Left err -> die err -- This cannot happen if the result was fewer than 1024 bytes in size
    Right compactRegion -> pure $ getCompact compactRegion
