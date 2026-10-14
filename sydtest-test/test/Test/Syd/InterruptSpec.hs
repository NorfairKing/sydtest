{-# LANGUAGE NumericUnderscores #-}

module Test.Syd.InterruptSpec (spec) where

import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Environment (getExecutablePath)
import System.Exit
import System.FilePath (takeDirectory, (</>))
import System.IO
import System.Process
import System.Timeout (timeout)
import Test.Syd

spec :: Spec
spec = describe "interrupting a test suite" $
  it "stops when Ctrl-C reaches a running test suite" $ do
    -- sydtest-ctrl-c-suite is an executable of this same package, so it is
    -- built whenever this test suite is, and it lands where a builder puts any
    -- component's executable: beside this one.  Nothing puts it on PATH, so
    -- there is nowhere else to look; if this path is wrong, spawning it says
    -- so and names it.
    thisSuite <- getExecutablePath
    let suiteExe =
          takeDirectory (takeDirectory thisSuite)
            </> "sydtest-ctrl-c-suite"
            </> "sydtest-ctrl-c-suite"
    -- Ctrl-C sends SIGINT to the terminal's foreground process group, so this
    -- runs a real test suite in a group of its own and signals that group.
    -- 'create_group' is what keeps the signal off this suite's own group.
    --
    -- Everything up to and including the exit code is asserted, because every
    -- step of it has gone wrong before: #80 was a suite that caught the
    -- interrupt and carried on, and a suite that treats it as a test failure
    -- retries the test, which is what the 600 second wait in the child makes
    -- impossible to mistake for an orderly stop.
    let suiteProcess =
          (proc suiteExe [])
            { std_out = CreatePipe,
              create_group = True
            }
    mExitCode <-
      timeout 30_000_000 $
        withCreateProcess suiteProcess $ \_ mOut _ processHandle -> case mOut of
          Nothing -> expectationFailure "could not capture the suite's output"
          Just out -> do
            -- Wait until a test is actually running, so the interrupt lands
            -- during one rather than during startup.
            let waitUntilRunningATest = do
                  eof <- hIsEOF out
                  if eof
                    then expectationFailure "the suite stopped before it started a test"
                    else do
                      line <- hGetLine out
                      unless ("sydtest-ctrl-c-suite is running a test" `isInfixOf` line) waitUntilRunningATest
            waitUntilRunningATest
            interruptProcessGroupOf processHandle
            waitForProcess processHandle
    -- A process that dies of the SIGINT it was sent reports the signal as a
    -- negative exit code, which is what the RTS re-raising it after
    -- 'UserInterrupt' reaches the top amounts to.
    mExitCode `shouldBe` Just (ExitFailure (-2))
