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
import Data.IORef
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.QuickCheck.IO ()
import Test.Syd.HList
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Runner.Synchronous
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Colour

runSpecForestAsynchronously :: Settings -> Word -> TestForest '[] () -> IO ResultForest
runSpecForestAsynchronously settings nbThreads testForest = do
  handleForest <- makeHandleForest testForest
  failFastVar <- newEmptyMVar
  let runRunner = runner settings nbThreads failFastVar handleForest
      runPrinter = liftIO $ waiter failFastVar handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  pure resultForest

runSpecForestInterleavedWithOutputAsynchronously :: Settings -> Word -> TestForest '[] () -> IO (Timed ResultForest)
runSpecForestInterleavedWithOutputAsynchronously settings nbThreads testForest = do
  handleForest <- makeHandleForest testForest
  failFastVar <- newEmptyMVar
  let runRunner = runner settings nbThreads failFastVar handleForest
      runPrinter = liftIO $ printer settings failFastVar handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  pure resultForest

type HandleForest a b = SpecDefForest a b (MVar (Timed TestRunResult))

type HandleTree a b = SpecDefTree a b (MVar (Timed TestRunResult))

makeHandleForest :: TestForest a b -> IO (HandleForest a b)
makeHandleForest = traverse $ traverse $ \() -> newEmptyMVar

runner :: Settings -> Word -> MVar () -> HandleForest '[] () -> IO ()
runner settings nbThreads failFastVar handleForest = do
  sem <- liftIO $ newQSemN $ fromIntegral nbThreads
  jobs <- newIORef (S.empty :: Set (Async ()))
  -- This is used to make sure that the 'after' part of the resources actually happens after the tests are done, not just when they are started.
  let waitForCurrentlyRunning :: IO ()
      waitForCurrentlyRunning = do
        as <- readIORef jobs
        mapM_ wait as
        writeIORef jobs S.empty
  let goForest :: Parallelism -> FlakinessMode -> HList a -> HandleForest a () -> IO ()
      goForest p fm a = mapM_ (goTree p fm a)
      goTree :: Parallelism -> FlakinessMode -> HList a -> HandleTree a () -> IO ()
      goTree p fm a = \case
        DefSpecifyNode _ td var -> do
          mDone <- tryReadMVar failFastVar
          case mDone of
            Nothing -> do
              let runNow = timeItT $ runSingleTestWithFlakinessMode noProgressReporter a td fm
              -- Wait before spawning a thread so that we don't spawn too many threads
              let quantity = case p of
                    -- When the test wants to be executed sequentially, we take n locks because we must make sure that
                    -- 1. no more other tests are still running.
                    -- 2. no other tests are started during execution.
                    Sequential -> nbThreads
                    Parallel -> 1
              liftIO $ waitQSemN sem $ fromIntegral quantity
              let job :: IO ()
                  job = do
                    result <- runNow
                    putMVar var result
                    when (settingFailFast settings && testRunResultStatus (timedValue result) == TestFailed) $ do
                      putMVar failFastVar ()
                      as <- readIORef jobs
                      mapM_ cancel as
                    liftIO $ signalQSemN sem $ fromIntegral quantity
              jobAsync <- async job
              modifyIORef jobs (S.insert jobAsync)
              link jobAsync
            Just () -> pure ()
        DefPendingNode _ _ -> pure ()
        DefDescribeNode _ sdf -> goForest p fm a sdf
        DefWrapNode func sdf -> func (goForest p fm a sdf >> waitForCurrentlyRunning)
        DefBeforeAllNode func sdf -> do
          b <- func
          goForest p fm (HCons b a) sdf
        DefAroundAllNode func sdf ->
          func (\b -> goForest p fm (HCons b a) sdf >> waitForCurrentlyRunning)
        DefAroundAllWithNode func sdf ->
          let HCons x _ = a
           in func (\b -> goForest p fm (HCons b a) sdf >> waitForCurrentlyRunning) x
        DefAfterAllNode func sdf -> goForest p fm a sdf `finally` (waitForCurrentlyRunning >> func a)
        DefParallelismNode p' sdf -> goForest p' fm a sdf
        DefRandomisationNode _ sdf -> goForest p fm a sdf
        DefFlakinessNode fm' sdf -> goForest p fm' a sdf
  goForest Parallel MayNotBeFlaky HNil handleForest

printer :: Settings -> MVar () -> HandleForest '[] () -> IO (Timed ResultForest)
printer settings failFastVar handleForest = do
  tc <- deriveTerminalCapababilities settings

  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = liftIO $ do
        putChunksLocaleWith tc lineChunks
        TIO.putStrLn ""

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
        DefFlakinessNode _ sdf -> fmap SubForestNode <$> goForest level sdf
  mapM_ outputLine outputTestsHeader
  resultForest <- timeItT $ fromMaybe [] <$> goForest 0 handleForest
  outputLine [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading settings (timedValue resultForest)
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
        DefFlakinessNode _ sdf -> fmap SubForestNode <$> goForest level sdf
  fromMaybe [] <$> goForest 0 handleForest
