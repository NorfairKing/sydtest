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
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import GHC.Clock (getMonotonicTimeNSec)
import Test.QuickCheck.IO ()
import Test.Syd.HList
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Runner.Single
import Test.Syd.Runner.Wrappers
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Colour

runSpecForestAsynchronously :: Settings -> Word -> TestForest '[] () -> IO (Timed ResultForest)
runSpecForestAsynchronously settings nbThreads testForest = do
  setPseudorandomness (settingSeed settings)
  handleForest <- makeHandleForest testForest
  abortVar <- newEmptyMVar
  resourceFailuresVar <- newTVarIO []
  let runRunner = runner settings nbThreads abortVar resourceFailuresVar handleForest
      runPrinter = liftIO $ waiter abortVar handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  resourceFailures <- readTVarIO resourceFailuresVar
  pure $ insertResourceFailures resourceFailures <$> resultForest

runSpecForestInterleavedWithOutputAsynchronously :: Settings -> Word -> TestForest '[] () -> IO (Timed ResultForest)
runSpecForestInterleavedWithOutputAsynchronously settings nbThreads testForest = do
  setPseudorandomness (settingSeed settings)
  handleForest <- makeHandleForest testForest
  abortVar <- newEmptyMVar
  resourceFailuresVar <- newTVarIO []
  suiteBegin <- getMonotonicTimeNSec
  let runRunner = runner settings nbThreads abortVar resourceFailuresVar handleForest
      runPrinter = case settingOutputFormat settings of
        OutputFormatPretty -> liftIO $ printer settings abortVar suiteBegin handleForest
        OutputFormatTerse -> liftIO $ waiter abortVar handleForest
  ((), resultForestWithoutResourceFailures) <- concurrently runRunner runPrinter
  resourceFailures <- readTVarIO resourceFailuresVar
  let resultForest = insertResourceFailures resourceFailures <$> resultForestWithoutResourceFailures

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

makeHandleForest :: TestForest a b -> IO (HandleForest a b)
makeHandleForest = traverse $ traverse $ \() -> newEmptyMVar

-- | Give every test in a forest a result saying a resource it needed failed.
--
-- 'tryPutMVar' because a test that already has a result keeps it: this is for
-- the tests that were never scheduled, and a resource failing on the way out
-- does not take the results of the tests it was around.
failTestsWith :: SomeException -> HandleForest a b -> IO ()
failTestsWith failure =
  traverse_ $
    traverse_ $ \var ->
      void $ tryPutMVar var $ zeroTimed $ resourceFailureReport failure

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
    -- Re-throwing it is what the synchronous runners do.
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

-- | Run an action, recording any exception that escapes it before letting it
-- propagate unchanged.
--
-- This is a tee rather than a handler: nothing is swallowed, so catching
-- 'SomeException' does not change which exceptions reach the caller, only
-- which one gets reported if several race.  It has to be this broad because
-- /any/ exception that escapes before a test's result is put leaves the
-- result-collecting thread waiting forever for that result.
recordEscapingExceptions :: MVar Abort -> IO a -> IO a
recordEscapingExceptions abortVar action =
  action `catch` \e -> do
    void $ tryPutMVar abortVar $ AbortException e
    throwIO (e :: SomeException)

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

-- | A resource that failed, and the descriptions above it.
--
-- The thread that catches one is not the thread that builds the result forest,
-- so these are collected here and put in the forest by path once both are
-- done.  See 'insertResourceFailure'.
type ResourceFailures = [([Text], Text, SomeException)]

runner :: Settings -> Word -> MVar Abort -> TVar ResourceFailures -> HandleForest '[] () -> IO ()
runner settings nbThreads abortVar resourceFailuresVar handleForest = do
  let nbWorkers = nbThreads
  let nbSpacesOnTheJobQueue = nbWorkers * 2
  jobQueue <- newJobQueue nbSpacesOnTheJobQueue

  withJobQueueWorkers nbWorkers jobQueue $ do
    let waitForWorkersDone :: IO ()
        waitForWorkersDone = blockUntilDone jobQueue

    -- Record a resource that failed, so that the thread collecting results can
    -- put it in the forest.
    let recordResourceFailure :: Text -> SomeException -> R a ()
        recordResourceFailure kind failure = do
          path <- asks eDescriptionPath
          liftIO $ atomically $ modifyTVar' resourceFailuresVar (++ [(path, kind, failure)])

    -- Report a resource that failed, and give the tests it never ran a result
    -- saying so.
    --
    -- The results are what keep the collecting thread going: it waits on each
    -- test in turn, so a subtree whose tests are never scheduled is a thread
    -- that never stops waiting.
    let reportResourceFailure :: Text -> HandleForest a' () -> SomeException -> R a ()
        reportResourceFailure kind hf failure = do
          recordResourceFailure kind failure
          liftIO $ failTestsWith failure hf

    let reportResourceOutcome :: Text -> HandleForest a' () -> ResourceOutcome () -> R a ()
        reportResourceOutcome kind hf outcome = case outcome of
          ResourceRan () -> pure ()
          ResourceFailedAfter () failure -> recordResourceFailure kind failure
          ResourceFailedBefore failure -> reportResourceFailure kind hf failure
          ResourceSkippedTests ->
            reportResourceFailure kind hf $
              toException (ExpectationFailed "This resource finished without ever running the tests inside it.")

    let goForest :: forall a. HandleForest a () -> R a ()
        goForest = mapM_ goTree

        goTree :: forall a. HandleTree a () -> R a ()
        goTree = \case
          DefSpecifyNode _ td var -> do
            -- Once the suite is aborting, we stop enqueuing jobs.
            mAbort <- liftIO $ tryReadMVar abortVar
            case mAbort of
              Just _ -> pure ()
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
                        result <- recordEscapingExceptions abortVar $ runNow workerNr

                        -- Put the result in the mvar.
                        --
                        -- 'tryPutMVar' because a resource that failed may have
                        -- put a result here already: scheduling can stop
                        -- part-way through a group, and the jobs already on the
                        -- queue still run.  Blocking on a full var would leave
                        -- this worker counted as working forever, which is the
                        -- hang this whole mechanism is here to avoid.  A result
                        -- that is already there says the resource this test
                        -- needed failed, which is the more useful of the two.
                        void $ tryPutMVar var result

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
          DefDescribeNode t sdf ->
            withReaderT (\e -> e {eDescriptionPath = eDescriptionPath e ++ [t]}) (goForest sdf)
          DefSetupNode func sdf -> do
            errOrUnit <- liftIO $ try func
            case errOrUnit of
              Left failure -> reportResourceFailure "beforeAll_" sdf failure
              Right () -> goForest sdf
          DefBeforeAllNode func sdf -> do
            errOrB <- liftIO $ try func
            case errOrB of
              Left failure -> reportResourceFailure "beforeAll" sdf failure
              Right b ->
                withReaderT
                  (\e -> e {eExternalResources = HCons b (eExternalResources e)})
                  (goForest sdf)
          DefBeforeAllWithNode func sdf -> do
            e <- ask
            let HCons x _ = eExternalResources e
            errOrB <- liftIO $ try $ func x
            case errOrB of
              Left failure -> reportResourceFailure "beforeAllWith" sdf failure
              Right b ->
                liftIO $
                  runReaderT
                    (goForest sdf)
                    (e {eExternalResources = HCons b (eExternalResources e)})
          DefWrapNode func sdf -> do
            e <- ask
            outcome <-
              liftIO $
                applySimpleWrapperReported'' func $ do
                  runReaderT (goForest sdf) e
                  waitForWorkersDone
            reportResourceOutcome "aroundAll_" sdf outcome
          DefAroundAllNode func sdf -> do
            e <- ask
            outcome <-
              liftIO $
                applySimpleWrapperReported'
                  func
                  ( \b -> do
                      runReaderT
                        (goForest sdf)
                        (e {eExternalResources = HCons b (eExternalResources e)})
                      waitForWorkersDone
                  )
            reportResourceOutcome "aroundAll" sdf outcome
          DefAroundAllWithNode func sdf -> do
            e <- ask
            let outers = eExternalResources e
            outcome <-
              liftIO $
                applySimpleWrapperReported
                  func
                  ( \b -> do
                      runReaderT
                        (goForest sdf)
                        (e {eExternalResources = HCons b outers})
                      waitForWorkersDone
                  )
                  outers
            reportResourceOutcome "aroundAllWith" sdf outcome
          DefAfterAllNode func sdf -> do
            e <- ask
            errOrUnit <- liftIO $ do
              -- The tests first, then the resource, and the resource even when
              -- scheduling them threw: that is what 'finally' did here, and a
              -- resource that is not torn down when something propagates is a
              -- resource leaked.
              errOrScheduled <- try $ runReaderT (goForest sdf) e
              errOrUnit <- try $ do
                waitForWorkersDone
                func (eExternalResources e)
              case errOrScheduled of
                Left (propagating :: SomeException) -> throwIO propagating
                Right () -> pure errOrUnit
            reportResourceOutcome "afterAll" sdf $ case errOrUnit of
              Right () -> ResourceRan ()
              Left failure -> ResourceFailedAfter () failure
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
        { eDescriptionPath = [],
          eParallelism = RunParallel,
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
  { eDescriptionPath :: ![Text],
    eParallelism :: !RunParallelism,
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
