{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Runner.Synchronous.Interleaved (runSpecForestInterleavedWithOutputSynchronously) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Syd.HList
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Runner.Single
import Test.Syd.Runner.Wrappers
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Colour

runSpecForestInterleavedWithOutputSynchronously :: Settings -> TestForest '[] () -> IO (Timed ResultForest)
runSpecForestInterleavedWithOutputSynchronously settings testForest = do
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = liftIO $ do
        putChunksLocaleWith (settingTerminalCapabilities settings) lineChunks
        TIO.putStrLn ""

      treeWidth :: Int
      treeWidth = specForestWidth testForest

  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.pack (replicate (paddingSize * level) ' ')) :)

  let outputLineR :: [Chunk] -> R a ()
      outputLineR line = do
        level <- asks eLevel
        liftIO $ outputLine $ pad level line

      outputLinesR :: [[Chunk]] -> R a ()
      outputLinesR cs = case settingOutputFormat settings of
        OutputFormatPretty -> mapM_ outputLineR cs
        OutputFormatTerse -> return ()

  let goForest :: TestForest a () -> R a (Next ResultForest)
      goForest [] = pure (Continue [])
      goForest (tt : rest) = do
        nrt <- goTree tt
        case nrt of
          Continue rt -> do
            nf <- goForest rest
            pure $ (rt :) <$> nf
          Stop rt -> pure $ Stop [rt]

      goTree :: TestTree a () -> R a (Next ResultTree)
      goTree = \case
        DefSpecifyNode t td () -> do
          Env {..} <- ask
          let progressReporter :: Progress -> IO ()
              progressReporter =
                outputLine . pad (succ (succ eLevel)) . \case
                  ProgressTestStarting ->
                    [ fore cyan "Test starting: ",
                      fore yellow $ chunk t
                    ]
                  ProgressExampleStarting totalExamples exampleNr ->
                    [ fore cyan "Example starting:  ",
                      fore yellow $ exampleNrChunk totalExamples exampleNr
                    ]
                  ProgressExampleDone totalExamples exampleNr executionTime ->
                    [ fore cyan "Example done:      ",
                      fore yellow $ exampleNrChunk totalExamples exampleNr,
                      timeChunkFor executionTime
                    ]
                  ProgressTestDone ->
                    [ fore cyan "Test done: ",
                      fore yellow $ chunk t
                    ]
          result <-
            liftIO $
              timeItT 0 $
                runSingleTestWithFlakinessMode
                  progressReporter
                  eExternalResources
                  td
                  eTimeout
                  eRetries
                  eFlakinessMode
                  eExpectationMode
          let td' = td {testDefVal = result}
          outputLinesR $ outputSpecifyLines settings eLevel treeWidth t td'
          let r = failFastNext settings td'
          pure $ SpecifyNode t <$> r
        DefPendingNode t mr -> do
          outputLinesR $ outputPendingLines t mr
          pure $ Continue $ PendingNode t mr
        DefDescribeNode t sdf -> do
          outputLineR $ outputDescribeLine t
          fmap (DescribeNode t) <$> addLevel (goForest sdf)
        DefSetupNode func sdf -> do
          liftIO func
          fmap SubForestNode <$> goForest sdf
        DefBeforeAllNode func sdf ->
          fmap SubForestNode
            <$> ( do
                    b <- liftIO func
                    withReaderT
                      (\e -> e {eExternalResources = HCons b (eExternalResources e)})
                      (goForest sdf)
                )
        DefBeforeAllWithNode func sdf -> do
          e <- ask
          let HCons x _ = eExternalResources e
          liftIO $
            fmap SubForestNode
              <$> ( do
                      b <- liftIO $ func x
                      runReaderT
                        (goForest sdf)
                        (e {eExternalResources = HCons b (eExternalResources e)})
                  )
        DefWrapNode func sdf -> do
          e <- ask
          liftIO $ fmap SubForestNode <$> applySimpleWrapper'' func (runReaderT (goForest sdf) e)
        DefAroundAllNode func sdf -> do
          e <- ask
          liftIO $
            fmap SubForestNode
              <$> applySimpleWrapper'
                func
                ( \b ->
                    runReaderT
                      (goForest sdf)
                      (e {eExternalResources = HCons b (eExternalResources e)})
                )
        DefAroundAllWithNode func sdf -> do
          e <- ask
          let outers = eExternalResources e
          liftIO $
            fmap SubForestNode
              <$> applySimpleWrapper
                func
                ( \b ->
                    runReaderT
                      (goForest sdf)
                      (e {eExternalResources = HCons b outers})
                )
                outers
        DefAfterAllNode func sdf -> do
          e <- ask
          let externalResources = eExternalResources e
          liftIO $ fmap SubForestNode <$> (runReaderT (goForest sdf) e `finally` func externalResources)
        DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest sdf -- Ignore, it's synchronous anyway
        DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest sdf -- Ignore, randomisation has already happened.
        DefTimeoutNode modTimeout sdf ->
          fmap SubForestNode
            <$> withReaderT
              (\e -> e {eTimeout = modTimeout (eTimeout e)})
              (goForest sdf)
        DefRetriesNode modRetries sdf ->
          fmap SubForestNode
            <$> withReaderT
              (\e -> e {eRetries = modRetries (eRetries e)})
              (goForest sdf)
        DefFlakinessNode fm sdf ->
          fmap SubForestNode
            <$> withReaderT
              (\e -> e {eFlakinessMode = fm})
              (goForest sdf)
        DefExpectationNode em sdf ->
          fmap SubForestNode
            <$> withReaderT
              (\e -> e {eExpectationMode = em})
              (goForest sdf)

  mapM_ outputLine outputTestsHeader
  resultForest <-
    timeItT 0 $
      extractNext
        <$> runReaderT
          (goForest testForest)
          Env
            { eLevel = 0,
              eTimeout = settingTimeout settings,
              eRetries = settingRetries settings,
              eFlakinessMode = MayNotBeFlaky,
              eExpectationMode = ExpectPassing,
              eExternalResources = HNil
            }

  outputLine [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading settings (timedValue resultForest)
  outputLine [chunk " "]
  mapM_ outputLine $ outputStats (computeTestSuiteStats settings <$> resultForest)
  outputLine [chunk " "]

  when (settingProfile settings) $ do
    mapM_ outputLine (outputProfilingInfo resultForest)
    outputLine [chunk " "]

  pure resultForest

addLevel :: R a b -> R a b
addLevel = withReaderT (\e -> e {eLevel = succ (eLevel e)})

type R a = ReaderT (Env a) IO

-- Not exported, on purpose.
data Env externalResources = Env
  { eLevel :: Int,
    eTimeout :: !Timeout,
    eRetries :: !Word,
    eFlakinessMode :: !FlakinessMode,
    eExpectationMode :: !ExpectationMode,
    eExternalResources :: !(HList externalResources)
  }
