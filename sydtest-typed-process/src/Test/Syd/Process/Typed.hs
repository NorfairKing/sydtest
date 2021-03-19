{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.Process.Typed where

import Control.Exception
import System.Process.Typed
import Test.Syd

-- | Run a given process while a test is running and give access to a process handle as an inner resource.
--
-- See 'typedProcessSetupFunc'.
typedProcessSpec :: ProcessConfig stdin stdout stderr -> TestDefM outers (Process stdin stdout stderr) result -> TestDefM outers () result
typedProcessSpec pc = setupAround $ typedProcessSetupFunc pc

-- | Run a given process while a group of tests is running and give access to a process handle as an outer resource.
--
-- See 'typedProcessSetupFunc'.
--
-- == __FOOTGUN__
--
-- The process will be shared accross multiple tests.
-- This may well be a good idea because starting the process can be prohibitively expensive to do around every test.
-- However, sharing the process means that tests could be sharing state.
-- When using this function, it is important to implement some form of cleaning the state before every test.
-- It is also important to use 'sequential' if some such state is maintained (and cleaned) so that the state is not cleaned while another test is running.
outerTypedProcessSpec :: ProcessConfig stdin stdout stderr -> TestDefM (Process stdin stdout stderr ': outers) inner result -> TestDefM outers inner result
outerTypedProcessSpec pc = setupAroundAll $ typedProcessSetupFunc pc

-- | Set up a process beforehand and stop it afterwards.
--
-- The process will be terminated using 'stopProcess'.
typedProcessSetupFunc :: ProcessConfig stdin stdout stderr -> SetupFunc () (Process stdin stdout stderr)
typedProcessSetupFunc pc =
  makeSimpleSetupFunc
    ( \func -> bracket (startProcess pc) stopProcess $ \ph -> do
        func ph
    )
