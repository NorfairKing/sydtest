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

import Control.Concurrent
import Control.Concurrent.Async as Async
import Control.Exception
import Control.Monad.Reader
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
  let runRunner = runner settings nbThreads failFastVar handleForest
      runPrinter = liftIO $ printer settings failFastVar handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  pure resultForest

type HandleForest a b = SpecDefForest a b (MVar (Timed TestRunReport))

type HandleTree a b = SpecDefTree a b (MVar (Timed TestRunReport))

makeHandleForest :: TestForest a b -> IO (HandleForest a b)
makeHandleForest = traverse $ traverse $ \() -> newEmptyMVar

runner :: Settings -> Word -> MVar () -> HandleForest '[] () -> IO ()
runner settings nbThreads failFastVar handleForest = do
  sem <- liftIO $ newQSemN $ fromIntegral nbThreads
  jobsVar <- newMVar (S.empty :: Set (Async ()))
  -- This is used to make sure that the 'after' part of the resources actually happens after the tests are done, not just when they are started.
  let waitForCurrentlyRunning :: IO ()
      waitForCurrentlyRunning = do
        modifyMVar_ jobsVar $ \jobThreads -> do
          mapM_ Async.wait jobThreads
          pure S.empty

  let goForest :: forall a. HandleForest a () -> R a ()
      goForest = mapM_ goTree

      goTree :: forall a. HandleTree a () -> R a ()
      goTree = \case
        DefSpecifyNode _ td var -> do
          -- If the fail-fast var has been put, This will return 'Just ()', in
          -- which case we must stop.
          mDoneEarly <- liftIO $ tryReadMVar failFastVar
          case mDoneEarly of
            Just () -> pure ()
            Nothing -> do
              Env {..} <- ask

              liftIO $ do
                -- Wait before spawning a thread so that we don't spawn too many threads
                let quantity = case eParallelism of
                      -- When the test wants to be executed sequentially, we take n locks because we must make sure that
                      -- 1. no more other tests are still running.
                      -- 2. no other tests are started during execution.
                      Sequential -> nbThreads
                      Parallel -> 1
                waitQSemN sem $ fromIntegral quantity

                let runNow = timeItT $ runSingleTestWithFlakinessMode noProgressReporter eExternalResources td eRetries eFlakinessMode
                let job :: IO ()
                    job = do
                      -- Start the test
                      result <- runNow

                      -- Put the result in the mvar
                      putMVar var result

                      -- If we should fail fast, put the fail-fast var and cancel all other jobs.
                      when (settingFailFast settings && testRunReportFailed settings (timedValue result)) $ do
                        putMVar failFastVar ()
                        withMVar jobsVar $ \jobThreads ->
                          mapM_ cancel jobThreads
                      liftIO $ signalQSemN sem $ fromIntegral quantity

                modifyMVar_ jobsVar $ \jobThreads -> do
                  jobThread <- async job
                  link jobThread
                  pure (S.insert jobThread jobThreads)
        DefPendingNode _ _ -> pure ()
        DefDescribeNode _ sdf -> goForest sdf
        DefWrapNode func sdf -> do
          e <- ask
          liftIO $
            func $ do
              runReaderT (goForest sdf) e
              waitForCurrentlyRunning
        DefBeforeAllNode func sdf -> do
          b <- liftIO func
          withReaderT
            (\e -> e {eExternalResources = HCons b (eExternalResources e)})
            (goForest sdf)
        DefAroundAllNode func sdf -> do
          e <- ask
          liftIO $
            func
              ( \b -> do
                  runReaderT
                    (goForest sdf)
                    (e {eExternalResources = HCons b (eExternalResources e)})
                  waitForCurrentlyRunning
              )
        DefAroundAllWithNode func sdf -> do
          e <- ask
          let HCons x _ = eExternalResources e
          liftIO $
            func
              ( \b -> do
                  runReaderT
                    (goForest sdf)
                    (e {eExternalResources = HCons b (eExternalResources e)})
                  waitForCurrentlyRunning
              )
              x
        DefAfterAllNode func sdf -> do
          e <- ask
          liftIO $
            runReaderT (goForest sdf) e
              `finally` ( do
                            waitForCurrentlyRunning
                            func (eExternalResources e)
                        )
        DefParallelismNode p' sdf ->
          withReaderT
            (\e -> e {eParallelism = p'})
            (goForest sdf)
        DefRandomisationNode _ sdf -> goForest sdf -- Ignore, randomisation has already happened.
        DefRetriesNode modRetries sdf ->
          withReaderT
            (\e -> e {eRetries = modRetries (eRetries e)})
            (goForest sdf)
        DefFlakinessNode fm' sdf ->
          withReaderT
            (\e -> e {eFlakinessMode = fm'})
            (goForest sdf)

  runReaderT
    (goForest handleForest)
    Env
      { eParallelism = Parallel,
        eRetries = settingRetries settings,
        eFlakinessMode = MayNotBeFlaky,
        eExternalResources = HNil
      }

type R a = ReaderT (Env a) IO

-- Not exported, on purpose.
data Env externalResources = Env
  { eParallelism :: !Parallelism,
    eRetries :: !Word,
    eFlakinessMode :: !FlakinessMode,
    eExternalResources :: !(HList externalResources)
  }

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
        DefWrapNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefBeforeAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAroundAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAroundAllWithNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAfterAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefRetriesNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefFlakinessNode _ sdf -> fmap SubForestNode <$> goForest sdf
  mapM_ outputLine outputTestsHeader
  resultForest <- timeItT $ fromMaybe [] <$> runReaderT (goForest handleForest) 0
  outputLine [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading settings (timedValue resultForest)
  outputLine [chunk " "]
  mapM_ outputLine $ outputStats (computeTestSuiteStats settings <$> resultForest)
  outputLine [chunk " "]
  pure resultForest

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
        DefWrapNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefBeforeAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAroundAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAroundAllWithNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefAfterAllNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefRetriesNode _ sdf -> fmap SubForestNode <$> goForest sdf
        DefFlakinessNode _ sdf -> fmap SubForestNode <$> goForest sdf
  fromMaybe [] <$> goForest handleForest
