{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines how to run a test suite
module Test.Syd.Runner.Asynchronous where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as SB8
import Data.IORef
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Test.QuickCheck.IO ()
import Test.Syd.HList
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Colour

runSpecForestAsynchronously :: Bool -> Int -> TestForest '[] () -> IO ResultForest
runSpecForestAsynchronously failFast nbThreads testForest = do
  handleForest <- makeHandleForest testForest
  failFastVar <- newEmptyMVar
  let runRunner = runner failFast nbThreads failFastVar handleForest
      runPrinter = liftIO $ waiter failFastVar handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  pure resultForest

runSpecForestInterleavedWithOutputAsynchronously :: TerminalCapabilities -> Bool -> Int -> TestForest '[] () -> IO (Timed ResultForest)
runSpecForestInterleavedWithOutputAsynchronously tc failFast nbThreads testForest = do
  handleForest <- makeHandleForest testForest
  failFastVar <- newEmptyMVar
  let runRunner = runner failFast nbThreads failFastVar handleForest
      runPrinter = liftIO $ printer tc failFastVar handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  pure resultForest

type HandleForest a b = SpecDefForest a b (MVar (Timed TestRunResult))

type HandleTree a b = SpecDefTree a b (MVar (Timed TestRunResult))

makeHandleForest :: TestForest a b -> IO (HandleForest a b)
makeHandleForest = traverse $
  traverse $ \() ->
    newEmptyMVar

runner :: Bool -> Int -> MVar () -> HandleForest '[] () -> IO ()
runner failFast nbThreads failFastVar handleForest = do
  sem <- liftIO $ newQSemN nbThreads
  jobs <- newIORef (S.empty :: Set (Async ()))
  -- This is used to make sure that the 'after' part of the resources actually happens after the tests are done, not just when they are started.
  let waitForCurrentlyRunning :: IO ()
      waitForCurrentlyRunning = do
        as <- readIORef jobs
        mapM_ wait as
        writeIORef jobs S.empty
  let goForest :: Parallelism -> HList a -> HandleForest a () -> IO ()
      goForest p a = mapM_ (goTree p a)
      goTree :: Parallelism -> HList a -> HandleTree a () -> IO ()
      goTree p a = \case
        DefSpecifyNode _ td var -> do
          mDone <- tryReadMVar failFastVar
          case mDone of
            Nothing -> do
              let runNow = timeItT $ testDefVal td (\f -> f a ())
              -- Wait before spawning a thread so that we don't spawn too many threads
              let quantity = case p of
                    -- When the test wants to be executed sequentially, we take n locks because we must make sure that
                    -- 1. no more other tests are still running.
                    -- 2. no other tests are started during execution.
                    Sequential -> nbThreads
                    Parallel -> 1
              liftIO $ waitQSemN sem quantity
              let job :: IO ()
                  job = do
                    result <- runNow
                    putMVar var result
                    when (failFast && testRunResultStatus (timedValue result) == TestFailed) $ do
                      putMVar failFastVar ()
                      as <- readIORef jobs
                      mapM_ cancel as
                    liftIO $ signalQSemN sem quantity
              jobAsync <- async job
              modifyIORef jobs (S.insert jobAsync)
              link jobAsync
            Just () -> pure ()
        DefPendingNode _ _ -> pure ()
        DefDescribeNode _ sdf -> goForest p a sdf
        DefWrapNode func sdf -> func (goForest p a sdf >> waitForCurrentlyRunning)
        DefBeforeAllNode func sdf -> do
          b <- func
          goForest p (HCons b a) sdf
        DefAroundAllNode func sdf ->
          func (\b -> goForest p (HCons b a) sdf >> waitForCurrentlyRunning)
        DefAroundAllWithNode func sdf ->
          let HCons x _ = a
           in func (\b -> goForest p (HCons b a) sdf >> waitForCurrentlyRunning) x
        DefAfterAllNode func sdf -> goForest p a sdf `finally` (waitForCurrentlyRunning >> func a)
        DefParallelismNode p' sdf -> goForest p' a sdf
        DefRandomisationNode _ sdf -> goForest p a sdf
  goForest Parallel HNil handleForest

printer :: TerminalCapabilities -> MVar () -> HandleForest '[] () -> IO (Timed ResultForest)
printer tc failFastVar handleForest = do
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = liftIO $ do
        putChunksWith tc lineChunks
        SB8.putStrLn ""

      treeWidth :: Int
      treeWidth = specForestWidth handleForest

  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.pack (replicate (paddingSize * level) ' ')) :)

  let goForest :: Int -> HandleForest a b -> IO (Maybe ResultForest)
      goForest level hts = do
        rts <- catMaybes <$> mapM (goTree level) hts
        pure $ if null rts then Nothing else Just rts

      goTree :: Int -> HandleTree a b -> IO (Maybe ResultTree)
      goTree level = \case
        DefSpecifyNode t td var -> do
          failFastOrResult <- race (readMVar failFastVar) (takeMVar var)
          case failFastOrResult of
            Left () -> pure Nothing
            Right result -> do
              let td' = td {testDefVal = result}
              mapM_ (outputLine . pad level) $ outputSpecifyLines level treeWidth t td'
              pure $ Just $ SpecifyNode t td'
        DefPendingNode t mr -> do
          mapM_ (outputLine . pad level) $ outputPendingLines t mr
          pure $ Just $ PendingNode t mr
        DefDescribeNode t sf -> do
          mDone <- tryReadMVar failFastVar
          case mDone of
            Nothing -> do
              outputLine $ pad level $ outputDescribeLine t
              fmap (DescribeNode t) <$> goForest (succ level) sf
            Just () -> pure Nothing
        DefWrapNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefBeforeAllNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefAroundAllNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefAroundAllWithNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefAfterAllNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest level sdf
  mapM_ outputLine outputTestsHeader
  resultForest <- timeItT $ fromMaybe [] <$> goForest 0 handleForest
  outputLine [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading (timedValue resultForest)
  outputLine [chunk " "]
  mapM_ outputLine $ outputStats (computeTestSuiteStats <$> resultForest)
  outputLine [chunk " "]
  pure resultForest

waiter :: MVar () -> HandleForest '[] () -> IO ResultForest
waiter failFastVar handleForest = do
  let goForest :: Int -> HandleForest a b -> IO (Maybe ResultForest)
      goForest level hts = do
        rts <- catMaybes <$> mapM (goTree level) hts
        pure $ if null rts then Nothing else Just rts

      goTree :: Int -> HandleTree a b -> IO (Maybe ResultTree)
      goTree level = \case
        DefSpecifyNode t td var -> do
          failFastOrResult <- race (readMVar failFastVar) (takeMVar var)
          case failFastOrResult of
            Left () -> pure Nothing
            Right result -> do
              let td' = td {testDefVal = result}
              pure $ Just $ SpecifyNode t td'
        DefPendingNode t mr -> pure $ Just $ PendingNode t mr
        DefDescribeNode t sf -> do
          fmap (DescribeNode t) <$> goForest (succ level) sf
        DefWrapNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefBeforeAllNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefAroundAllNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefAroundAllWithNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefAfterAllNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest level sdf
        DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest level sdf
  fromMaybe [] <$> goForest 0 handleForest
