{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines how to run a test suite
module Test.Syd.Runner.Asynchronous
  ( runSpecForestAsynchronously,
    runSpecForestInterleavedWithOutputAsynchronously,
  )
where

import Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM as STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word
import GHC.Clock (getMonotonicTimeNSec)
import Test.QuickCheck.IO ()
import Test.Syd.HList
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Runner.Single
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
  suiteBegin <- getMonotonicTimeNSec
  let runRunner = runner settings nbThreads failFastVar handleForest
      runPrinter = liftIO $ printer settings failFastVar suiteBegin handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  pure resultForest

type HandleForest a b = SpecDefForest a b (MVar (Timed TestRunReport))

type HandleTree a b = SpecDefTree a b (MVar (Timed TestRunReport))

makeHandleForest :: TestForest a b -> IO (HandleForest a b)
makeHandleForest = traverse $ traverse $ \() -> newEmptyMVar

type Job = Int -> IO ()

-- | Job queue for workers that can synchronise
data JobQueue = JobQueue
  { -- | Bounded channel for the jobs.
    -- We use a TBQueue because it's bounded and we can check if it's empty.
    jobQueueTBQueue :: !(TBQueue Job),
    -- | Count of the number of job currently executed by workers.
    jobQueueWorkingCount :: !(TVar Int)
  }

-- | Make a new job queue with a given capacity
newJobQueue :: Word -> IO JobQueue
newJobQueue spots = do
  jobQueueTBQueue <- newTBQueueIO (fromIntegral spots)
  jobQueueWorkingCount <- newTVarIO (fromIntegral (0 :: Word))
  pure JobQueue {..}

-- | Enqueue a job, block until that's possible.
enqueueJob :: JobQueue -> Job -> IO ()
enqueueJob JobQueue {..} job =
  atomically $ writeTBQueue jobQueueTBQueue job

-- | Dequeue a job.
dequeueJob :: JobQueue -> STM Job
dequeueJob JobQueue {..} =
  readTBQueue jobQueueTBQueue

-- | Block until all workers are done (waiting to dequeue a job).
blockUntilDone :: JobQueue -> IO ()
blockUntilDone JobQueue {..} = do
  atomically $ do
    -- Wait until the queue is empty.
    isEmptyTBQueue jobQueueTBQueue >>= STM.check
    -- Wait for all workers to stop working.
    currentlyRunning <- readTVar jobQueueWorkingCount
    when (currentlyRunning > 0) retry

-- No new work can be started now, because the queue is empty and no worker
-- are running.

withJobQueueWorkers :: Word -> JobQueue -> IO a -> IO a
withJobQueueWorkers nbWorkers jobQueue func =
  withAsync
    ( mapConcurrently
        (jobQueueWorker jobQueue)
        [0 .. fromIntegral nbWorkers - 1]
    )
    (\_ -> func)

jobQueueWorker :: JobQueue -> Int -> IO ()
jobQueueWorker jobQueue workerIx = do
  forever $ do
    bracket
      ( atomically $ do
          -- Pick a job in queue and increase the count
          modifyTVar' (jobQueueWorkingCount jobQueue) (+ 1)
          dequeueJob jobQueue
      )
      (\_ -> atomically $ modifyTVar' (jobQueueWorkingCount jobQueue) (subtract 1))
      (\job -> job workerIx)

-- The plan is as follows:
--
-- We have:
--
-- 1 runner thread that schedules jobs
-- 1 waiter/printer thread that waits for the jobs to be done and puts them in
--   the result forest.
-- n worker threads that run the jobs.
--
-- Any outer resource might need cleanup, so whenever the scheduler thread
-- finishes an outer-resource subtree, it must wait for all tasks until then to
-- be completed before running the cleanup action.
--
-- There might be an ungodly number of tests so, to keep memory usage
-- contained, we want to limit the number of jobs that the scheduler can put on
-- the queue.
--
-- Tests may be marked as sequential, in which case only one test may be
-- executing at a time.
--
--
-- 1. We use a job queue semaphore that holds the number of empty
--    spots left on the queue.
--    The scheduler must wait for one unit of the semaphore before
--    enqueuing a job.
--    Any dequeuing must signal this semaphore
--
-- 2. We use a global lock for any job marked as "sequential".
--
--
-- The runner goes through the test 'HandleForest' one by one, and:
--
-- 1. Tries to enqueue as many jobs as possible.
--    It's only allowed to enqueue a jobs if there is space left on
--    the queue as indicated by the job semaphore.
--
-- 2. Asks workers to wait after finishing what they were doing at the end of
--     an outer resource block.
runner :: Settings -> Word -> MVar () -> HandleForest '[] () -> IO ()
runner settings nbThreads failFastVar handleForest = do
  let nbWorkers = nbThreads
  let nbSpacesOnTheJobQueue = nbWorkers * 2
  jobQueue <- newJobQueue nbSpacesOnTheJobQueue

  withJobQueueWorkers nbWorkers jobQueue $ do
    let waitForWorkersDone :: IO ()
        waitForWorkersDone = blockUntilDone jobQueue

    let goForest :: forall a. HandleForest a () -> R a ()
        goForest = mapM_ goTree

        goTree :: forall a. HandleTree a () -> R a ()
        goTree = \case
          DefSpecifyNode _ td var -> do
            -- If the fail-fast var has been put, we stop enqueuing jobs.
            mDoneEarly <- liftIO $ tryReadMVar failFastVar
            case mDoneEarly of
              Just () -> pure ()
              Nothing -> do
                Env {..} <- ask

                liftIO $ do
                  let runNow workerNr =
                        timeItT workerNr $
                          runSingleTestWithFlakinessMode
                            noProgressReporter
                            eExternalResources
                            td
                            eTimeout
                            eRetries
                            eFlakinessMode
                            eExpectationMode

                  let job :: Int -> IO ()
                      job workerNr = do
                        -- Start the test
                        result <- runNow workerNr

                        -- Put the result in the mvar
                        putMVar var result

                        -- If we should fail fast, put the
                        -- fail-fast var so that no new
                        -- jobs are started by the
                        -- scheduler.
                        when
                          ( settingFailFast settings
                              && testRunReportFailed settings (timedValue result)
                          )
                          $ do
                            putMVar failFastVar ()

                  -- When enqueuing a sequential job, make sure all workers are
                  -- done before and after.
                  -- It's not enough to just not have two tests running at the
                  -- same time, because they also need to be executed in order.
                  case eParallelism of
                    Sequential -> do
                      waitForWorkersDone
                      job 0
                    Parallel -> do
                      enqueueJob jobQueue job
          DefPendingNode _ _ -> pure ()
          DefDescribeNode _ sdf -> goForest sdf
          DefSetupNode func sdf -> do
            liftIO func
            goForest sdf
          DefBeforeAllNode func sdf -> do
            b <- liftIO func
            withReaderT
              (\e -> e {eExternalResources = HCons b (eExternalResources e)})
              (goForest sdf)
          DefBeforeAllWithNode func sdf -> do
            e <- ask
            let HCons x _ = eExternalResources e
            b <- liftIO $ func x
            liftIO $
              runReaderT
                (goForest sdf)
                (e {eExternalResources = HCons b (eExternalResources e)})
          DefWrapNode func sdf -> do
            e <- ask
            liftIO $
              func $ do
                runReaderT (goForest sdf) e
                waitForWorkersDone
          DefAroundAllNode func sdf -> do
            e <- ask
            liftIO $
              func
                ( \b -> do
                    runReaderT
                      (goForest sdf)
                      (e {eExternalResources = HCons b (eExternalResources e)})
                    waitForWorkersDone
                )
          DefAroundAllWithNode func sdf -> do
            e <- ask
            let outers = eExternalResources e
            liftIO $
              func
                ( \b -> do
                    runReaderT
                      (goForest sdf)
                      (e {eExternalResources = HCons b outers})
                    waitForWorkersDone
                )
                outers
          DefAfterAllNode func sdf -> do
            e <- ask
            liftIO $
              runReaderT (goForest sdf) e
                `finally` ( do
                              waitForWorkersDone
                              func (eExternalResources e)
                          )
          DefParallelismNode p' sdf ->
            withReaderT
              (\e -> e {eParallelism = p'})
              (goForest sdf)
          DefRandomisationNode _ sdf ->
            goForest sdf -- Ignore, randomisation has already happened.
          DefTimeoutNode modTimeout sdf ->
            withReaderT
              (\e -> e {eTimeout = modTimeout (eTimeout e)})
              (goForest sdf)
          DefRetriesNode modRetries sdf ->
            withReaderT
              (\e -> e {eRetries = modRetries (eRetries e)})
              (goForest sdf)
          DefFlakinessNode fm sdf ->
            withReaderT
              (\e -> e {eFlakinessMode = fm})
              (goForest sdf)
          DefExpectationNode em sdf ->
            withReaderT
              (\e -> e {eExpectationMode = em})
              (goForest sdf)

    runReaderT
      (goForest handleForest)
      Env
        { eParallelism = Parallel,
          eTimeout = settingTimeout settings,
          eRetries = settingRetries settings,
          eFlakinessMode = MayNotBeFlaky,
          eExpectationMode = ExpectPassing,
          eExternalResources = HNil
        }
    waitForWorkersDone -- Make sure all jobs are done before cancelling the runners.

type R a = ReaderT (Env a) IO

-- Not exported, on purpose.
data Env externalResources = Env
  { eParallelism :: !Parallelism,
    eTimeout :: !Timeout,
    eRetries :: !Word,
    eFlakinessMode :: !FlakinessMode,
    eExpectationMode :: !ExpectationMode,
    eExternalResources :: !(HList externalResources)
  }

printer :: Settings -> MVar () -> Word64 -> HandleForest '[] () -> IO (Timed ResultForest)
printer settings failFastVar suiteBegin handleForest = do
  tc <- deriveTerminalCapababilities settings

  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = liftIO $ do
        putChunksLocaleWith tc lineChunks
        TIO.putStrLn ""

      treeWidth :: Int
      treeWidth = specForestWidth handleForest

  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.pack (replicate (paddingSize * level) ' ')) :)

  let outputLineP :: [Chunk] -> P ()
      outputLineP line = do
        level <- ask
        liftIO $ outputLine $ pad level line

      outputLinesP :: [[Chunk]] -> P ()
      outputLinesP = mapM_ outputLineP

  let goForest :: HandleForest a b -> P (Maybe ResultForest)
      goForest hts = do
        rts <- catMaybes <$> mapM goTree hts
        pure $ if null rts then Nothing else Just rts

      goTree :: HandleTree a b -> P (Maybe ResultTree)
      goTree = \case
        DefSpecifyNode t td var -> do
          failFastOrResult <-
            liftIO $
              race
                (readMVar failFastVar)
                (takeMVar var)
          case failFastOrResult of
            Left () -> pure Nothing
            Right result -> do
              let td' = td {testDefVal = result}
              level <- ask
              outputLinesP $ outputSpecifyLines settings level treeWidth t td'
              pure $ Just $ SpecifyNode t td'
        DefPendingNode t mr -> do
          outputLinesP $ outputPendingLines t mr
          pure $ Just $ PendingNode t mr
        DefDescribeNode t sf -> do
          mDoneEarly <- liftIO $ tryReadMVar failFastVar
          case mDoneEarly of
            Just () -> pure Nothing
            Nothing -> do
              outputLineP $ outputDescribeLine t
              fmap (DescribeNode t) <$> addLevel (goForest sf)
        DefSetupNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefBeforeAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefBeforeAllWithNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefWrapNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAroundAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAroundAllWithNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAfterAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefTimeoutNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefRetriesNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefFlakinessNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefExpectationNode _ sdf -> fmap SubForestNode <$> goForest sdf
  mapM_ outputLine outputTestsHeader
  resultForest <- fromMaybe [] <$> runReaderT (goForest handleForest) 0
  outputLine [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading settings resultForest
  outputLine [chunk " "]
  suiteEnd <- getMonotonicTimeNSec
  let timedResult =
        Timed
          { timedValue = resultForest,
            timedWorker = 0,
            timedBegin = suiteBegin,
            timedEnd = suiteEnd
          }
  mapM_ outputLine $ outputStats (computeTestSuiteStats settings <$> timedResult)
  outputLine [chunk " "]

  when (settingProfile settings) $ do
    mapM_ outputLine (outputProfilingInfo timedResult)
    outputLine [chunk " "]

  pure timedResult

addLevel :: P a -> P a
addLevel = withReaderT succ

type P = ReaderT Int IO

waiter :: MVar () -> HandleForest '[] () -> IO ResultForest
waiter failFastVar handleForest = do
  let goForest :: HandleForest a b -> IO (Maybe ResultForest)
      goForest hts = do
        rts <- catMaybes <$> mapM goTree hts
        pure $ if null rts then Nothing else Just rts

      goTree :: HandleTree a b -> IO (Maybe ResultTree)
      goTree = \case
        DefSpecifyNode t td var -> do
          failFastOrResult <-
            race
              (readMVar failFastVar)
              (takeMVar var)
          case failFastOrResult of
            Left () -> pure Nothing
            Right result -> do
              let td' = td {testDefVal = result}
              pure $ Just $ SpecifyNode t td'
        DefPendingNode t mr -> pure $ Just $ PendingNode t mr
        DefDescribeNode t sf -> do
          fmap (DescribeNode t) <$> goForest sf
        DefSetupNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefBeforeAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefBeforeAllWithNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefWrapNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAroundAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAroundAllWithNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAfterAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefTimeoutNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefRetriesNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefFlakinessNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefExpectationNode _ sdf -> fmap SubForestNode <$> goForest sdf
  fromMaybe [] <$> goForest handleForest
