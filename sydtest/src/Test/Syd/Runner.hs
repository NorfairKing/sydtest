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
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as SB8
import System.Environment
import System.Mem (performGC)
import Test.Syd.Def
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Runner.Asynchronous
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef
import Text.Colour
import Text.Colour.Capabilities.FromEnv
import Text.Printf

sydTestResult :: Settings -> TestDefM '[] () r -> IO (Timed ResultForest)
sydTestResult sets spec = do
  let totalIterations = case settingIterations sets of
        OneIteration -> Just 1
        Iterations i -> Just i
        Continuous -> Nothing
  case totalIterations of
    Just 1 -> sydTestOnce sets spec
    _ -> sydTestIterations totalIterations sets spec

sydTestOnce :: Settings -> TestDefM '[] () r -> IO (Timed ResultForest)
sydTestOnce sets spec = do
  specForest <- execTestDefM sets spec
  tc <- case settingColour sets of
    Just False -> pure WithoutColours
    Just True -> pure With8BitColours
    Nothing -> getTerminalCapabilitiesFromEnv
  withArgs [] $ case settingThreads sets of
    Synchronous -> runSpecForestInterleavedWithOutputSynchronously tc (settingFailFast sets) specForest
    ByCapabilities -> do
      i <- getNumCapabilities

      when (i == 1) $ do
        let outputLine :: [Chunk] -> IO ()
            outputLine lineChunks = liftIO $ do
              putChunksWith tc lineChunks
              SB8.putStrLn ""
        mapM_
          ( outputLine
              . (: [])
              . fore red
          )
          [ chunk "WARNING: Only one CPU core detected, make sure to compile your test suite with these ghc options:",
            chunk "         -threaded -rtsopts -with-rtsopts=-N",
            chunk "         (This is important for correctness as well as speed, as a parallell test suite can find thread safety problems.)"
          ]
      runSpecForestInterleavedWithOutputAsynchronously tc (settingFailFast sets) i specForest
    Asynchronous i ->
      runSpecForestInterleavedWithOutputAsynchronously tc (settingFailFast sets) i specForest

sydTestIterations :: Maybe Int -> Settings -> TestDefM '[] () r -> IO (Timed ResultForest)
sydTestIterations totalIterations sets spec =
  withArgs [] $ do
    nbCapabilities <- getNumCapabilities

    let runOnce sets_ = do
          specForest <- execTestDefM sets_ spec
          r <- timeItT $ case settingThreads sets_ of
            Synchronous -> runSpecForestSynchronously (settingFailFast sets_) specForest
            ByCapabilities -> runSpecForestAsynchronously (settingFailFast sets_) nbCapabilities specForest
            Asynchronous i -> runSpecForestAsynchronously (settingFailFast sets_) i specForest
          performGC -- Just to be sure that nothing dangerous is lurking around in memory anywhere
          pure r

    let go iteration = do
          newSeedSetting <- case settingSeed sets of
            FixedSeed seed -> do
              let newSeed = seed + iteration
              putStrLn $ printf "Running iteration: %4d with seed %4d" iteration newSeed
              pure $ FixedSeed newSeed
            RandomSeed -> do
              putStrLn $ printf "Running iteration: %4d with random seeds" iteration
              pure RandomSeed
          rf <- runOnce $ sets {settingSeed = newSeedSetting}
          if shouldExitFail (timedValue rf)
            then pure rf
            else case totalIterations of
              Nothing -> go $ succ iteration
              Just i
                | iteration >= i -> pure rf
                | otherwise -> go $ succ iteration

    rf <- go 0
    tc <- case settingColour sets of
      Just False -> pure WithoutColours
      Just True -> pure With8BitColours
      Nothing -> getTerminalCapabilitiesFromEnv
    printOutputSpecForest tc rf
    pure rf
