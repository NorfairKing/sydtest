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
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.Functor.Compose
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

sydTest :: Spec -> IO ()
sydTest spec = do
  resultForest <- sydTestResult spec
  when (shouldExitFail resultForest) (exitWith (ExitFailure 1))

sydTestResult :: Spec -> IO ResultForest
sydTestResult spec = do
  ((), specForest) <- runTestDefM spec
  runSpecForestInterleavedWithOutputAsynchronously 10 specForest

runSpecForest :: TestForest -> IO ResultForest
runSpecForest = traverse $ traverse $ \td -> do
  let runFunc = testDefVal td
  result <- runFunc
  pure $ td {testDefVal = result}

runSpecForestInterleavedWithOutputSynchronously :: TestForest -> IO ResultForest
runSpecForestInterleavedWithOutputSynchronously testForest = do
  byteStringMaker <- byteStringMakerFromEnvironment
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = do
        let bss = chunksToByteStrings byteStringMaker lineChunks
        mapM_ SB.putStr bss
        SB8.putStrLn ""
  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.replicate (level * 2) " ") :)
      goTree :: Int -> TestTree -> IO ResultTree
      goTree level = \case
        DescribeNode t sts -> do
          outputLine $ pad level $ outputDescribeLine t
          DescribeNode t <$> goForest (succ level) sts
        SpecifyNode t td -> do
          let runFunc = testDefVal td
          result <- runFunc
          let td' = td {testDefVal = result}
          mapM_ (outputLine . pad level) $ outputSpecifyLines t td'
          pure $ SpecifyNode t td'
      goForest :: Int -> TestForest -> IO ResultForest
      goForest level = mapM (goTree level)
  mapM_ outputLine $ outputTestsHeader
  resultForest <- goForest 0 testForest
  outputLine $ [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading resultForest
  pure resultForest

runSpecForestInterleavedWithOutputAsynchronously :: Int -> TestForest -> IO ResultForest
runSpecForestInterleavedWithOutputAsynchronously nbThreads testForest = do
  handleForest <- makeHandleForest testForest
  let doOutput = outputter handleForest
      doRun = runner nbThreads handleForest
  ((), resultForest) <- concurrently doRun doOutput
  pure resultForest

type HandleForest = SpecForest (TestDef (IO TestRunResult), MVar TestRunResult)

type HandleTree = SpecTree (TestDef (IO TestRunResult), MVar TestRunResult)

makeHandleForest :: TestForest -> IO HandleForest
makeHandleForest = traverse $ traverse $ \td -> do
  var <- newEmptyMVar
  pure (td, var)

outputter :: HandleForest -> IO ResultForest
outputter handleForest = do
  byteStringMaker <- byteStringMakerFromEnvironment
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = do
        let bss = chunksToByteStrings byteStringMaker lineChunks
        mapM_ SB.putStr bss
        SB8.putStrLn ""
  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.replicate (level * 2) " ") :)
      goTree :: Int -> HandleTree -> IO ResultTree
      goTree level = \case
        DescribeNode t sts -> do
          outputLine $ pad level $ outputDescribeLine t
          DescribeNode t <$> goForest (succ level) sts
        SpecifyNode t (td, var) -> do
          result <- readMVar var
          let td' = td {testDefVal = result}
          mapM_ (outputLine . pad level) $ outputSpecifyLines t td'
          pure $ SpecifyNode t td'
      goForest :: Int -> HandleForest -> IO ResultForest
      goForest level = mapM (goTree level)
  mapM_ outputLine $ outputTestsHeader
  resultForest <- goForest 0 handleForest
  outputLine $ [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading resultForest
  pure resultForest

runner :: Int -> HandleForest -> IO ()
runner nbThreads handleForest = pooledMapConcurrentlyN_ nbThreads (uncurry runOne) (Compose handleForest)

runOne :: TestDef (IO TestRunResult) -> MVar TestRunResult -> IO ()
runOne td var = do
  a <- async $ testDefVal td
  result <- wait a
  putMVar var result

shouldExitFail :: ResultForest -> Bool
shouldExitFail = any (any ((== TestFailed) . testRunResultStatus . testDefVal))
