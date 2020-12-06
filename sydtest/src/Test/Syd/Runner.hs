{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines how to run a test suite
module Test.Syd.Runner
  ( module Test.Syd.Runner,
    module Test.Syd.Runner.Asynchronous,
    module Test.Syd.Runner.Synchronous,
  )
where

import Control.Concurrent (getNumCapabilities)
import Test.Syd.Def
import Test.Syd.OptParse
import Test.Syd.Runner.Asynchronous
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef

sydTestResult :: Settings -> TestDefM '[] () r -> IO ResultForest
sydTestResult sets spec = do
  specForest <- execTestDefM sets spec
  case settingThreads sets of
    Synchronous -> runSpecForestInterleavedWithOutputSynchronously specForest
    ByCapabilities -> do
      i <- getNumCapabilities
      runSpecForestInterleavedWithOutputAsynchronously i specForest
    Asynchronous i ->
      runSpecForestInterleavedWithOutputAsynchronously i specForest
