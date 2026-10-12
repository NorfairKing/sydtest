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
import Control.Concurrent.QSem
import Control.Concurrent.STM as STM
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Text as T
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

runSpecForestAsynchronously :: Settings -> Word -> TestForest '[] () -> IO (Timed ResultForest)
runSpecForestAsynchronously settings nbThreads testForest = do
  setPseudorandomness (settingSeed settings)
  handleForest <- makeHandleForest testForest
  abortVar <- newEmptyMVar
  let runRunner = runner settings nbThreads abortVar handleForest
      runPrinter = liftIO $ waiter abortVar handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  pure resultForest

runSpecForestInterleavedWithOutputAsynchronously :: Settings -> Word -> TestForest '[] () -> IO (Timed ResultForest)
runSpecForestInterleavedWithOutputAsynchronously settings nbThreads testForest = do
  setPseudorandomness (settingSeed settings)
  handleForest <- makeHandleForest testForest
  abortVar <- newEmptyMVar
  suiteBegin <- getMonotonicTimeNSec
  let runRunner = runner settings nbThreads abortVar handleForest
      runPrinter = case settingOutputFormat settings of
        OutputFormatPretty -> liftIO $ printer settings abortVar suiteBegin handleForest
        OutputFormatTerse -> liftIO $ waiter abortVar handleForest
  ((), resultForest) <- concurrently runRunner runPrinter

  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks =
        liftIO $
          -- Emit UTF-8 bytes directly so output never depends on the handle's
          -- locale encoding (a C/POSIX-locale handle is ASCII and would crash on
          -- the non-ASCII status markers).  The trailing newline is folded into
          -- the chunks so this stays a single byte-level write.
          putChunksUtf8With (settingTerminalCapabilities settings) (lineChunks <> [chunk "\n"])
      outputLines :: [[Chunk]] -> IO ()
      outputLines = mapM_ outputLine

  outputLines $ case settingOutputFormat settings of
    OutputFormatPretty -> outputPrettySummary settings resultForest
    OutputFormatTerse -> outputTerseSummary settings resultForest

  pure resultForest

type HandleForest a b = SpecDefForest a b (MVar (Timed TestRunReport))

type HandleTree a b = SpecDefTree a b (MVar (Timed TestRunReport))

-- | Why the suite stops before every test has produced a result.
--
-- Both reasons mean the runner must stop scheduling tests, but they differ in
-- what the result-collecting thread owes the caller: fail-fast is an orderly
-- stop, whereas an escaped exception leaves nothing to report.
data Abort
  = -- | The user asked for fail-fast and a test failed.
    AbortFailFast
  | -- | An exception escaped a test function, so the test that threw it will
    -- never deliver a result and no test queued behind it will either.
    --
    -- 'Test.Syd.Run.exceptionHandlers' re-throws asynchronously-classified
    -- exceptions on purpose, so this is the normal way for such an exception
    -- to reach the caller.  Re-throwing it is what the synchronous runners do.
    AbortException !SomeException

-- | Wait for a test's result, unless the suite is being aborted.
--
-- 'Nothing' means the suite stopped early, so this test has no result.
awaitTestResult :: MVar Abort -> MVar (Timed TestRunReport) -> IO (Maybe (Timed TestRunReport))
awaitTestResult abortVar var = do
  abortOrResult <- race (readMVar abortVar) (takeMVar var)
  case abortOrResult of
    Left AbortFailFast -> pure Nothing
    Left (AbortException e) -> throwIO e
    Right result -> pure (Just result)

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
runner :: Settings -> Word -> MVar Abort -> HandleForest '[] () -> IO ()
runner settings nbThreads abortVar handleForest = do
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
            -- Once the suite is aborting, we stop enqueuing jobs.
            mAbort <- liftIO $ tryReadMVar abortVar
            case mAbort of
              Just AbortFailFast -> pure ()
              -- The result-collecting thread re-throws, so there is nothing
              -- left for the runner to do but stop.
              Just (AbortException _) -> pure ()
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
                        -- Start the test.
                        --
                        -- An exception can still escape the test function:
                        -- 'Test.Syd.Run.exceptionHandlers' re-throws
                        -- asynchronously-classified ones instead of turning
                        -- them into a result.  Recording it before it kills
                        -- this worker is what keeps the result-collecting
                        -- thread from waiting forever for a result that will
                        -- never be put.
                        result <-
                          runNow workerNr `catch` \e -> do
                            void $ tryPutMVar abortVar $ AbortException e
                            throwIO (e :: SomeException)

                        -- Put the result in the mvar
                        putMVar var result

                        -- If we should fail fast, put the
                        -- abort var so that no new
                        -- jobs are started by the
                        -- scheduler.
                        when
                          ( settingFailFast settings
                              && testRunReportFailed settings (timedValue result)
                          )
                          $ do
                            -- 'tryPutMVar' because another worker may be
                            -- failing fast at the same time, and blocking here
                            -- would leave this worker counted as working
                            -- forever.
                            void $ tryPutMVar abortVar AbortFailFast

                  -- When enqueuing a sequential job, make sure all workers are
                  -- done before and after.
                  -- It's not enough to just not have two tests running at the
                  -- same time, because they also need to be executed in order.
                  case eParallelism of
                    RunSequential -> do
                      waitForWorkersDone
                      job 0
                    RunParallel -> do
                      enqueueJob jobQueue job
                    RunParallelWith sem ->
                      -- Still queued like any other job, so this only ever
                      -- holds tests back; it never runs more of them than
                      -- there are workers.
                      enqueueJob jobQueue $ \workerNr ->
                        bracket_ (waitQSem sem) (signalQSem sem) (job workerNr)
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
          DefParallelismNode p' sdf -> do
            runParallelism <- liftIO $ resolveParallelism p'
            withReaderT
              (\e -> e {eParallelism = runParallelism})
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
        { eParallelism = RunParallel,
          eTimeout = settingTimeout settings,
          eRetries = settingRetries settings,
          eFlakinessMode = MayNotBeFlaky,
          eExpectationMode = ExpectPassing,
          eExternalResources = HNil
        }
    waitForWorkersDone -- Make sure all jobs are done before cancelling the runners.

-- | 'Parallelism', with the semaphore a bound needs already made.
--
-- One semaphore per 'DefParallelismNode', so everything below that node shares
-- the one bound rather than each group getting its own.
data RunParallelism
  = RunParallel
  | RunParallelWith !QSem
  | RunSequential

resolveParallelism :: Parallelism -> IO RunParallelism
resolveParallelism = \case
  Parallel -> pure RunParallel
  ParallelWith w -> RunParallelWith <$> newQSem (fromIntegral w)
  Sequential -> pure RunSequential

type R a = ReaderT (Env a) IO

-- Not exported, on purpose.
data Env externalResources = Env
  { eParallelism :: !RunParallelism,
    eTimeout :: !Timeout,
    eRetries :: !Word,
    eFlakinessMode :: !FlakinessMode,
    eExpectationMode :: !ExpectationMode,
    eExternalResources :: !(HList externalResources)
  }

printer :: Settings -> MVar Abort -> Word64 -> HandleForest '[] () -> IO (Timed ResultForest)
printer settings abortVar suiteBegin handleForest = do
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks =
        liftIO $
          -- Emit UTF-8 bytes directly so output never depends on the handle's
          -- locale encoding (a C/POSIX-locale handle is ASCII and would crash on
          -- the non-ASCII status markers).  The trailing newline is folded into
          -- the chunks so this stays a single byte-level write.
          putChunksUtf8With (settingTerminalCapabilities settings) (lineChunks <> [chunk "\n"])

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
          mResult <- liftIO $ awaitTestResult abortVar var
          case mResult of
            Nothing -> pure Nothing
            Just result -> do
              let td' = td {testDefVal = result}
              level <- ask
              outputLinesP $ outputSpecifyLines settings level treeWidth t td'
              pure $ Just $ SpecifyNode t td'
        DefPendingNode t mr -> do
          outputLinesP $ outputPendingLines t mr
          pure $ Just $ PendingNode t mr
        DefDescribeNode t sf -> do
          mAbort <- liftIO $ tryReadMVar abortVar
          case mAbort of
            Just AbortFailFast -> pure Nothing
            Just (AbortException e) -> liftIO $ throwIO e
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
  suiteEnd <- getMonotonicTimeNSec
  let timedResult =
        Timed
          { timedValue = resultForest,
            timedWorker = 0,
            timedBegin = suiteBegin,
            timedEnd = suiteEnd
          }

  pure timedResult

addLevel :: P a -> P a
addLevel = withReaderT succ

type P = ReaderT Int IO

waiter :: MVar Abort -> HandleForest '[] () -> IO (Timed ResultForest)
waiter abortVar handleForest = timeItT 0 $ do
  let goForest :: HandleForest a b -> IO (Maybe ResultForest)
      goForest hts = do
        rts <- catMaybes <$> mapM goTree hts
        pure $ if null rts then Nothing else Just rts

      goTree :: HandleTree a b -> IO (Maybe ResultTree)
      goTree = \case
        DefSpecifyNode t td var -> do
          mResult <- awaitTestResult abortVar var
          case mResult of
            Nothing -> pure Nothing
            Just result -> do
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
