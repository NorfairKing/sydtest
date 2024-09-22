{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Data.Text.IO as TIO
import System.Environment
import System.Mem (performGC)
import System.Random (mkStdGen, setStdGen)
import Test.Syd.Def
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Runner.Asynchronous
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef
import Text.Colour
import Text.Printf

-- | Set the command line argument of the underlying action to empty.
--
-- The action behaves as if no command line argument were provided. Especially,
-- it removes all the arguments initially provided to sydtest and provides a
-- reproducible environment.
setNullArgs :: IO a -> IO a
setNullArgs action = do
  -- Check that args are not empty before setting it to empty.
  -- This is a workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/18261
  -- In summary, `withArgs` is not thread-safe, hence we would like to avoid it
  -- as much as possible.
  --
  -- If sydtest is used in a more complex environment which may use `withArgs`
  -- too, we would like to avoid a complete crash of the program.
  --
  -- Especially, if sydtest is used itself in a sydtest test (e.g. in order to
  -- test sydtest command line itself), it may crash, see
  -- https://github.com/NorfairKing/sydtest/issues/91 for details.
  args <- getArgs
  if null args
    then action
    else withArgs [] action

sydTestResult :: Settings -> TestDefM '[] () r -> IO (Timed ResultForest)
sydTestResult settings spec = do
  let totalIterations = case settingIterations settings of
        OneIteration -> Just 1
        Iterations i -> Just i
        Continuous -> Nothing
  case totalIterations of
    Just 1 -> sydTestOnce settings spec
    _ -> sydTestIterations totalIterations settings spec

sydTestOnce :: Settings -> TestDefM '[] () r -> IO (Timed ResultForest)
sydTestOnce settings spec = do
  specForest <- execTestDefM settings spec
  tc <- deriveTerminalCapababilities settings
  setNullArgs $ do
    setPseudorandomness (settingSeed settings)
    case settingThreads settings of
      Synchronous -> runSpecForestInterleavedWithOutputSynchronously settings specForest
      ByCapabilities -> do
        i <- fromIntegral <$> getNumCapabilities

        when (i == 1) $ do
          let outputLine :: [Chunk] -> IO ()
              outputLine lineChunks = liftIO $ do
                putChunksLocaleWith tc lineChunks
                TIO.putStrLn ""
          mapM_
            ( outputLine
                . (: [])
                . fore red
            )
            [ chunk "WARNING: Only one CPU core detected, make sure to compile your test suite with these ghc options:",
              chunk "         -threaded -rtsopts -with-rtsopts=-N",
              chunk "         (This is important for correctness as well as speed, as a parallel test suite can find thread safety problems.)"
            ]
        runSpecForestInterleavedWithOutputAsynchronously settings i specForest
      Asynchronous i ->
        runSpecForestInterleavedWithOutputAsynchronously settings i specForest

sydTestIterations :: Maybe Word -> Settings -> TestDefM '[] () r -> IO (Timed ResultForest)
sydTestIterations totalIterations settings spec =
  setNullArgs $ do
    nbCapabilities <- fromIntegral <$> getNumCapabilities

    let runOnce settings_ = do
          setPseudorandomness (settingSeed settings_)
          specForest <- execTestDefM settings_ spec
          r <- timeItT 0 $ case settingThreads settings_ of
            Synchronous -> runSpecForestSynchronously settings_ specForest
            ByCapabilities -> runSpecForestAsynchronously settings_ nbCapabilities specForest
            Asynchronous i -> runSpecForestAsynchronously settings_ i specForest
          performGC -- Just to be sure that nothing dangerous is lurking around in memory anywhere
          pure r

    let go iteration = do
          newSeedSetting <- case settingSeed settings of
            FixedSeed seed -> do
              let newSeed = seed + fromIntegral iteration
              putStrLn $ printf "Running iteration: %4d with seed %4d" iteration newSeed
              pure $ FixedSeed newSeed
            RandomSeed -> do
              putStrLn $ printf "Running iteration: %4d with random seeds" iteration
              pure RandomSeed
          rf <- runOnce $ settings {settingSeed = newSeedSetting}
          if shouldExitFail settings (timedValue rf)
            then pure rf
            else case totalIterations of
              Nothing -> go $ succ iteration
              Just i
                | iteration >= i -> pure rf
                | otherwise -> go $ succ iteration

    rf <- go 0
    printOutputSpecForest settings rf
    pure rf

setPseudorandomness :: SeedSetting -> IO ()
setPseudorandomness = \case
  RandomSeed -> pure ()
  FixedSeed seed -> setStdGen (mkStdGen seed)
