{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd
  ( module Test.Syd,
    module Test.Syd.Run,
    module Test.Syd.SpecForest,
    module Test.Syd.Expectation,
    module Test.Syd.Output,
    module Test.Syd.Silence,
    module Test.Syd.Def,
  )
where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.Functor.Compose
import Data.IORef
import qualified Data.Text as T
import Rainbow
import System.Exit
import Test.QuickCheck.IO ()
import Test.Syd.Def
import Test.Syd.Expectation
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Silence
import Test.Syd.SpecForest
import UnliftIO.Async
import UnliftIO.Resource

sydTest :: Spec -> IO ()
sydTest spec = do
  resultForest <- sydTestResult spec
  when (shouldExitFail resultForest) (exitWith (ExitFailure 1))

sydTestResult :: Spec -> IO ResultForest
sydTestResult spec = do
  ((), specForest) <- runTestDefM spec
  runSpecForestInterleavedWithOutputSynchronously specForest

runSpecForestSynchronously :: TestForest -> ResourceT IO ResultForest
runSpecForestSynchronously = traverse $ traverse $ \td -> do
  let runFunc = testDefVal td
  result <- runFunc
  pure $ td {testDefVal = result}

runSpecForestInterleavedWithOutputSynchronously :: TestForest -> IO ResultForest
runSpecForestInterleavedWithOutputSynchronously testForest = runResourceT $ do
  byteStringMaker <- liftIO byteStringMakerFromEnvironment
  let outputLine :: [Chunk] -> ResourceT IO ()
      outputLine lineChunks = do
        let bss = chunksToByteStrings byteStringMaker lineChunks
        liftIO $ do
          mapM_ SB.putStr bss
          SB8.putStrLn ""
  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.replicate (level * 2) " ") :)
      goTree :: Int -> TestTree -> ResourceT IO ResultTree
      goTree level = \case
        DescribeNode t sf -> do
          outputLine $ pad level $ outputDescribeLine t
          DescribeNode t <$> goForest (succ level) sf
        SpecifyNode t td -> do
          let runFunc = testDefVal td
          result <- runFunc
          let td' = td {testDefVal = result}
          mapM_ (outputLine . pad level) $ outputSpecifyLines t td'
          pure $ SpecifyNode t td'
      goForest :: Int -> TestForest -> ResourceT IO ResultForest
      goForest level = mapM (goTree level)
  mapM_ outputLine $ outputTestsHeader
  resultForest <- goForest 0 testForest
  outputLine $ [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading resultForest
  pure resultForest

shouldExitFail :: ResultForest -> Bool
shouldExitFail = any (any ((== TestFailed) . testRunResultStatus . testDefVal))
