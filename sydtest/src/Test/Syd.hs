{-# LANGUAGE LambdaCase #-}
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

import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
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

sydTest :: Spec -> IO ()
sydTest spec = do
  resultForest <- sydTestResult spec
  when (shouldExitFail resultForest) (exitWith (ExitFailure 1))

sydTestResult :: Spec -> IO ResultForest
sydTestResult spec = do
  ((), specForest) <- runTestDefM spec
  runSpecForestInterleavedWithOutput specForest

runSpecForest :: TestForest -> IO ResultForest
runSpecForest = traverse $ traverse $ \td -> do
  let runFunc = testDefVal td
  result <- runFunc
  pure $ td {testDefVal = result}

runSpecForestInterleavedWithOutput :: TestForest -> IO ResultForest
runSpecForestInterleavedWithOutput testForest = do
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

shouldExitFail :: ResultForest -> Bool
shouldExitFail = any (any ((== TestFailed) . testRunResultStatus . testDefVal))
