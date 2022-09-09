{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module defines how to run a test suite
module Test.Syd.Runner.Synchronous where

import Control.Exception
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Syd.HList
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Runner.Wrappers
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Colour

runSpecForestSynchronously :: Settings -> TestForest '[] () -> IO ResultForest
runSpecForestSynchronously settings = fmap extractNext . goForest (settingRetries settings) MayNotBeFlaky HNil
  where
    goForest :: Word -> FlakinessMode -> HList a -> TestForest a () -> IO (Next ResultForest)
    goForest _ _ _ [] = pure (Continue [])
    goForest retries f hl (tt : rest) = do
      nrt <- goTree retries f hl tt
      case nrt of
        Continue rt -> do
          nf <- goForest retries f hl rest
          pure $ (rt :) <$> nf
        Stop rt -> pure $ Stop [rt]
    goTree :: forall a. Word -> FlakinessMode -> HList a -> TestTree a () -> IO (Next ResultTree)
    goTree retries fm hl = \case
      DefSpecifyNode t td () -> do
        result <- timeItT $ runSingleTestWithFlakinessMode noProgressReporter hl td retries fm
        let td' = td {testDefVal = result}
        let r = failFastNext settings td'
        pure $ SpecifyNode t <$> r
      DefPendingNode t mr -> pure $ Continue $ PendingNode t mr
      DefDescribeNode t sdf -> fmap (DescribeNode t) <$> goForest retries fm hl sdf
      DefWrapNode func sdf -> fmap SubForestNode <$> applySimpleWrapper'' func (goForest retries fm hl sdf)
      DefBeforeAllNode func sdf -> do
        fmap SubForestNode
          <$> ( do
                  b <- func
                  goForest retries fm (HCons b hl) sdf
              )
      DefAroundAllNode func sdf ->
        fmap SubForestNode <$> applySimpleWrapper' func (\b -> goForest retries fm (HCons b hl) sdf)
      DefAroundAllWithNode func sdf ->
        let HCons x _ = hl
         in fmap SubForestNode <$> applySimpleWrapper func (\b -> goForest retries fm (HCons b hl) sdf) x
      DefAfterAllNode func sdf -> fmap SubForestNode <$> (goForest retries fm hl sdf `finally` func hl)
      DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest retries fm hl sdf -- Ignore, it's synchronous anyway
      DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest retries fm hl sdf
      DefRetriesNode modRetries sdf -> fmap SubForestNode <$> goForest (modRetries retries) fm hl sdf
      DefFlakinessNode fm' sdf -> fmap SubForestNode <$> goForest retries fm' hl sdf

runSpecForestInterleavedWithOutputSynchronously :: Settings -> TestForest '[] () -> IO (Timed ResultForest)
runSpecForestInterleavedWithOutputSynchronously settings testForest = do
  tc <- deriveTerminalCapababilities settings
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = liftIO $ do
        putChunksLocaleWith tc lineChunks
        TIO.putStrLn ""
      treeWidth :: Int
      treeWidth = specForestWidth testForest
  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.pack (replicate (paddingSize * level) ' ')) :)
      goForest :: Int -> Word -> FlakinessMode -> HList a -> TestForest a () -> IO (Next ResultForest)
      goForest _ _ _ _ [] = pure (Continue [])
      goForest level retries fm l (tt : rest) = do
        nrt <- goTree level retries fm l tt
        case nrt of
          Continue rt -> do
            nf <- goForest level retries fm l rest
            pure $ (rt :) <$> nf
          Stop rt -> pure $ Stop [rt]
      goTree :: Int -> Word -> FlakinessMode -> HList a -> TestTree a () -> IO (Next ResultTree)
      goTree level retries fm hl = \case
        DefSpecifyNode t td () -> do
          let progressReporter :: Progress -> IO ()
              progressReporter =
                outputLine . pad (succ (succ level)) . \case
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
          result <- timeItT $ runSingleTestWithFlakinessMode progressReporter hl td retries fm
          let td' = td {testDefVal = result}
          mapM_ (outputLine . pad level) $ outputSpecifyLines settings level treeWidth t td'
          let r = failFastNext settings td'
          pure $ SpecifyNode t <$> r
        DefPendingNode t mr -> do
          mapM_ (outputLine . pad level) $ outputPendingLines t mr
          pure $ Continue $ PendingNode t mr
        DefDescribeNode t sf -> do
          outputLine $ pad level $ outputDescribeLine t
          fmap (DescribeNode t) <$> goForest (succ level) retries fm hl sf
        DefWrapNode func sdf -> fmap SubForestNode <$> applySimpleWrapper'' func (goForest level retries fm hl sdf)
        DefBeforeAllNode func sdf ->
          fmap SubForestNode
            <$> ( do
                    b <- func
                    goForest level retries fm (HCons b hl) sdf
                )
        DefAroundAllNode func sdf ->
          fmap SubForestNode <$> applySimpleWrapper' func (\b -> goForest level retries fm (HCons b hl) sdf)
        DefAroundAllWithNode func sdf ->
          let HCons x _ = hl
           in fmap SubForestNode <$> applySimpleWrapper func (\b -> goForest level retries fm (HCons b hl) sdf) x
        DefAfterAllNode func sdf -> fmap SubForestNode <$> (goForest level retries fm hl sdf `finally` func hl)
        DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest level retries fm hl sdf -- Ignore, it's synchronous anyway
        DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest level retries fm hl sdf
        DefRetriesNode modRetries sdf -> fmap SubForestNode <$> goForest level (modRetries retries) fm hl sdf
        DefFlakinessNode fm' sdf -> fmap SubForestNode <$> goForest level retries fm' hl sdf
  mapM_ outputLine outputTestsHeader
  resultForest <- timeItT $ extractNext <$> goForest 0 (settingRetries settings) MayNotBeFlaky HNil testForest
  outputLine [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading settings (timedValue resultForest)
  outputLine [chunk " "]
  mapM_ outputLine $ outputStats (computeTestSuiteStats settings <$> resultForest)
  outputLine [chunk " "]

  pure resultForest

-- | Run a single test.
--
-- Run the test up to 'maxRetries' times.
-- Finish as soon as the test passes once, or when we run out of retries.
runSingleTestWithFlakinessMode ::
  forall externalResources t.
  -- | How to report test progress
  ProgressReporter ->
  -- | External resources
  HList externalResources ->
  -- | Test definition
  TDef
    ( ProgressReporter ->
      ((HList externalResources -> () -> t) -> t) ->
      IO TestRunResult
    ) ->
  -- | Max retries
  Word ->
  -- | Flakiness mode
  FlakinessMode ->
  -- | Test result
  IO TestRunReport
runSingleTestWithFlakinessMode progressReporter l td maxRetries fm = do
  results <- runSingleTestWithRetries progressReporter l td maxRetries
  pure
    TestRunReport
      { testRunReportRawResults = results,
        testRunReportFlakinessMode = fm
      }

runSingleTestWithRetries ::
  forall externalResources t.
  -- | How to report test progress
  ProgressReporter ->
  -- | External resources
  HList externalResources ->
  -- | Test definition
  TDef
    ( ProgressReporter ->
      ((HList externalResources -> () -> t) -> t) ->
      IO TestRunResult
    ) ->
  -- | Max retries
  Word ->
  -- If the test ever passed, and the last test result
  IO (NonEmpty TestRunResult)
runSingleTestWithRetries progressReporter l td maxRetries = go maxRetries
  where
    go :: Word -> IO (NonEmpty TestRunResult)
    go w
      | w <= 1 = (:| []) <$> runFunc
      | otherwise = do
        result <- runFunc
        case testRunResultStatus result of
          TestPassed -> pure (result :| [])
          TestFailed -> do
            rest <- go (pred w)
            pure (result NE.<| rest)
      where
        runFunc :: IO TestRunResult
        runFunc = testDefVal td progressReporter (\f -> f l ())
