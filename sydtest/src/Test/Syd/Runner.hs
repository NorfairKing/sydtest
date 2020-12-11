{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines how to run a test suite
module Test.Syd.Runner
  ( module Test.Syd.Runner,
    module Test.Syd.Runner.Asynchronous,
    module Test.Syd.Runner.Synchronous,
  )
where

import Control.Concurrent (getNumCapabilities)
import System.Environment
import Test.Syd.Def
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.Runner.Asynchronous
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef

sydTestResult :: Settings -> TestDefM '[] () r -> IO (Timed ResultForest)
sydTestResult sets spec = do
  specForest <- execTestDefM sets spec
  withArgs [] $ do
    case settingThreads sets of
      Synchronous -> runSpecForestInterleavedWithOutputSynchronously (settingColour sets) (settingFailFast sets) specForest
      ByCapabilities -> do
        i <- getNumCapabilities
        runSpecForestInterleavedWithOutputAsynchronously (settingColour sets) (settingFailFast sets) i specForest
      Asynchronous i ->
        runSpecForestInterleavedWithOutputAsynchronously (settingColour sets) (settingFailFast sets) i specForest
