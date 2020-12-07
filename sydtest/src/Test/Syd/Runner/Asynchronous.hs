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
module Test.Syd.Runner.Asynchronous where

import Control.Concurrent.QSem
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Text as T
import Rainbow
import Test.QuickCheck.IO ()
import Test.Syd.HList
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import UnliftIO

runSpecForestAsynchronously :: Int -> TestForest '[] () -> IO ResultForest
runSpecForestAsynchronously nbThreads testForest = do
  handleForest <- makeHandleForest testForest
  let runRunner = runner nbThreads handleForest
      runPrinter = liftIO $ waiter handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  pure resultForest

runSpecForestInterleavedWithOutputAsynchronously :: Int -> TestForest '[] () -> IO ResultForest
runSpecForestInterleavedWithOutputAsynchronously nbThreads testForest = do
  handleForest <- makeHandleForest testForest
  let runRunner = runner nbThreads handleForest
      runPrinter = liftIO $ printer handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  pure resultForest

type HandleForest a b = SpecDefForest a b (MVar TestRunResult)

type HandleTree a b = SpecDefTree a b (MVar TestRunResult)

makeHandleForest :: TestForest a b -> IO (HandleForest a b)
makeHandleForest = traverse $
  traverse $ \() -> do
    var <- newEmptyMVar
    pure var

runner :: Int -> HandleForest '[] () -> IO ()
runner nbThreads handleForest = do
  sem <- liftIO $ newQSem nbThreads
  let goForest :: Parallelism -> HList a -> HandleForest a () -> IO ()
      goForest p a = mapM_ (goTree p a)
      goTree :: Parallelism -> HList a -> HandleTree a () -> IO ()
      goTree p a = \case
        DefDescribeNode _ sdf -> goForest p a sdf
        DefSpecifyNode _ td var -> do
          let runNow = testDefVal td (\f -> f a ())
          case p of
            Parallel -> do
              liftIO $ waitQSem sem
              let job :: IO ()
                  job = do
                    result <- runNow
                    putMVar var result
                    liftIO $ signalQSem sem
              jobAsync <- async job
              link jobAsync
            Sequential -> do
              result <- runNow
              putMVar var result
        DefWrapNode func sdf -> func (goForest p a sdf)
        DefBeforeAllNode func sdf -> do
          b <- func
          goForest p (HCons b a) sdf
        DefAroundAllNode func sdf ->
          func (\b -> goForest p (HCons b a) sdf)
        DefAroundAllWithNode func sdf ->
          let HCons x _ = a
           in func (\b -> goForest p (HCons b a) sdf) x
        DefAfterAllNode func sdf -> goForest p a sdf `finally` func a
        DefParallelismNode p' sdf -> goForest p' a sdf
  goForest Parallel HNil handleForest

printer :: HandleForest '[] () -> IO ResultForest
printer handleForest = do
  byteStringMaker <- liftIO byteStringMakerFromEnvironment
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = do
        let bss = chunksToByteStrings byteStringMaker lineChunks
        mapM_ SB.putStr bss
        SB8.putStrLn ""
      treeWidth :: Int
      treeWidth = specForestWidth handleForest
  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.pack (replicate (paddingSize * level) ' ')) :)
      goTree :: Int -> HandleTree a b -> IO ResultTree
      goTree level = \case
        DefDescribeNode t sf -> do
          outputLine $ pad level $ outputDescribeLine t
          DescribeNode t <$> goForest (succ level) sf
        DefSpecifyNode t td var -> do
          result <- takeMVar var
          let td' = td {testDefVal = result}
          mapM_ (outputLine . pad level) $ outputSpecifyLines level treeWidth t td'
          pure $ SpecifyNode t td'
        DefWrapNode _ sdf -> SubForestNode <$> goForest level sdf
        DefBeforeAllNode _ sdf -> SubForestNode <$> goForest level sdf
        DefAroundAllNode _ sdf -> SubForestNode <$> goForest level sdf
        DefAroundAllWithNode _ sdf -> SubForestNode <$> goForest level sdf
        DefAfterAllNode _ sdf -> SubForestNode <$> goForest level sdf
        DefParallelismNode _ sdf -> SubForestNode <$> goForest level sdf
      goForest :: Int -> HandleForest a b -> IO ResultForest
      goForest level = mapM (goTree level)
  mapM_ outputLine $ outputTestsHeader
  resultForest <- goForest 0 handleForest
  outputLine $ [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading resultForest
  pure resultForest

waiter :: HandleForest '[] () -> IO ResultForest
waiter handleForest = do
  let goTree :: Int -> HandleTree a b -> IO ResultTree
      goTree level = \case
        DefDescribeNode t sf -> do
          DescribeNode t <$> goForest (succ level) sf
        DefSpecifyNode t td var -> do
          result <- takeMVar var
          let td' = td {testDefVal = result}
          pure $ SpecifyNode t td'
        DefWrapNode _ sdf -> SubForestNode <$> goForest level sdf
        DefBeforeAllNode _ sdf -> SubForestNode <$> goForest level sdf
        DefAroundAllNode _ sdf -> SubForestNode <$> goForest level sdf
        DefAroundAllWithNode _ sdf -> SubForestNode <$> goForest level sdf
        DefAfterAllNode _ sdf -> SubForestNode <$> goForest level sdf
        DefParallelismNode _ sdf -> SubForestNode <$> goForest level sdf
      goForest :: Int -> HandleForest a b -> IO ResultForest
      goForest level = mapM (goTree level)
  goForest 0 handleForest
