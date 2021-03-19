{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.Process where

import Control.Exception
import System.IO
import System.Process
import Test.Syd

-- | Run a given process while a test is running and give access to a process handle as an inner resource.
--
-- See 'processSetupFunc'.
processSpec :: CreateProcess -> TestDefM outers (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) result -> TestDefM outers () result
processSpec cp = setupAround $ processSetupFunc cp

-- | Run a given process while a group of tests is running and give access to a process handle as an outer resource.
--
-- See 'processSetupFunc'.
--
-- == __FOOTGUN__
--
-- The process will be shared accross multiple tests.
-- This may well be a good idea because starting the process can be prohibitively expensive to do around every test.
-- However, sharing the process means that tests could be sharing state.
-- When using this function, it is important to implement some form of cleaning the state before every test.
-- It is also important to use 'sequential' if some such state is maintained (and cleaned) so that the state is not cleaned while another test is running.
outerProcessSpec :: CreateProcess -> TestDefM ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) ': outers) inner result -> TestDefM outers inner result
outerProcessSpec cp = setupAroundAll $ processSetupFunc cp

-- | Set up a process beforehand and stop it afterwards.
--
-- The process will be terminated using 'cleanupProcess'.
processSetupFunc :: CreateProcess -> SetupFunc () (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
processSetupFunc cp =
  makeSimpleSetupFunc
    ( \func -> bracket (createProcess cp) cleanupProcess $ \ph -> do
        func ph
    )
