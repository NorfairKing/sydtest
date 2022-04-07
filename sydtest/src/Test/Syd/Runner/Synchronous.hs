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
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Text as T
import Test.Syd.HList
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Runner.Wrappers
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Colour

runSpecForestSynchronously :: Settings -> TestForest '[] () -> IO ResultForest
runSpecForestSynchronously settings = fmap extractNext . goForest MayNotBeFlaky HNil
  where
    goForest :: FlakinessMode -> HList a -> TestForest a () -> IO (Next ResultForest)
    goForest _ _ [] = pure (Continue [])
    goForest f hl (tt : rest) = do
      nrt <- goTree f hl tt
      case nrt of
        Continue rt -> do
          nf <- goForest f hl rest
          pure $ (rt :) <$> nf
        Stop rt -> pure $ Stop [rt]
    goTree :: forall a. FlakinessMode -> HList a -> TestTree a () -> IO (Next ResultTree)
    goTree fm hl = \case
      DefSpecifyNode t td () -> do
        result <- timeItT $ runSingleTestWithFlakinessMode noProgressReporter hl td fm
        let td' = td {testDefVal = result}
        let r = failFastNext (settingFailFast settings) td'
        pure $ SpecifyNode t <$> r
      DefPendingNode t mr -> pure $ Continue $ PendingNode t mr
      DefDescribeNode t sdf -> fmap (DescribeNode t) <$> goForest fm hl sdf
      DefWrapNode func sdf -> fmap SubForestNode <$> applySimpleWrapper'' func (goForest fm hl sdf)
      DefBeforeAllNode func sdf -> do
        fmap SubForestNode
          <$> ( do
                  b <- func
                  goForest fm (HCons b hl) sdf
              )
      DefAroundAllNode func sdf ->
        fmap SubForestNode <$> applySimpleWrapper' func (\b -> goForest fm (HCons b hl) sdf)
      DefAroundAllWithNode func sdf ->
        let HCons x _ = hl
         in fmap SubForestNode <$> applySimpleWrapper func (\b -> goForest fm (HCons b hl) sdf) x
      DefAfterAllNode func sdf -> fmap SubForestNode <$> (goForest fm hl sdf `finally` func hl)
      DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest fm hl sdf -- Ignore, it's synchronous anyway
      DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest fm hl sdf
      DefFlakinessNode fm' sdf -> fmap SubForestNode <$> goForest fm' hl sdf

runSpecForestInterleavedWithOutputSynchronously :: Settings -> TestForest '[] () -> IO (Timed ResultForest)
runSpecForestInterleavedWithOutputSynchronously settings testForest = do
  tc <- deriveTerminalCapababilities settings
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = liftIO $ do
        putChunksWith tc lineChunks
        SB8.putStrLn ""
      treeWidth :: Int
      treeWidth = specForestWidth testForest
  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.pack (replicate (paddingSize * level) ' ')) :)
      goForest :: Int -> FlakinessMode -> HList a -> TestForest a () -> IO (Next ResultForest)
      goForest _ _ _ [] = pure (Continue [])
      goForest level fm l (tt : rest) = do
        nrt <- goTree level fm l tt
        case nrt of
          Continue rt -> do
            nf <- goForest level fm l rest
            pure $ (rt :) <$> nf
          Stop rt -> pure $ Stop [rt]
      goTree :: Int -> FlakinessMode -> HList a -> TestTree a () -> IO (Next ResultTree)
      goTree level fm hl = \case
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
          result <- timeItT $ runSingleTestWithFlakinessMode progressReporter hl td fm
          let td' = td {testDefVal = result}
          mapM_ (outputLine . pad level) $ outputSpecifyLines level treeWidth t td'
          let r = failFastNext (settingFailFast settings) td'
          pure $ SpecifyNode t <$> r
        DefPendingNode t mr -> do
          mapM_ (outputLine . pad level) $ outputPendingLines t mr
          pure $ Continue $ PendingNode t mr
        DefDescribeNode t sf -> do
          outputLine $ pad level $ outputDescribeLine t
          fmap (DescribeNode t) <$> goForest (succ level) fm hl sf
        DefWrapNode func sdf -> fmap SubForestNode <$> applySimpleWrapper'' func (goForest level fm hl sdf)
        DefBeforeAllNode func sdf ->
          fmap SubForestNode
            <$> ( do
                    b <- func
                    goForest level fm (HCons b hl) sdf
                )
        DefAroundAllNode func sdf ->
          fmap SubForestNode <$> applySimpleWrapper' func (\b -> goForest level fm (HCons b hl) sdf)
        DefAroundAllWithNode func sdf ->
          let HCons x _ = hl
           in fmap SubForestNode <$> applySimpleWrapper func (\b -> goForest level fm (HCons b hl) sdf) x
        DefAfterAllNode func sdf -> fmap SubForestNode <$> (goForest level fm hl sdf `finally` func hl)
        DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest level fm hl sdf -- Ignore, it's synchronous anyway
        DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest level fm hl sdf
        DefFlakinessNode fm' sdf -> fmap SubForestNode <$> goForest level fm' hl sdf
  mapM_ outputLine outputTestsHeader
  resultForest <- timeItT $ extractNext <$> goForest 0 MayNotBeFlaky HNil testForest
  outputLine [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading settings (timedValue resultForest)
  outputLine [chunk " "]
  mapM_ outputLine $ outputStats (computeTestSuiteStats <$> resultForest)
  outputLine [chunk " "]

  pure resultForest

runSingleTestWithFlakinessMode :: forall a t. ProgressReporter -> HList a -> TDef (ProgressReporter -> ((HList a -> () -> t) -> t) -> IO TestRunResult) -> FlakinessMode -> IO TestRunResult
runSingleTestWithFlakinessMode progressReporter l td = \case
  MayNotBeFlaky -> runFunc
  MayBeFlakyUpTo retries mMsg -> updateFlakinessMessage <$> go retries
    where
      updateFlakinessMessage :: TestRunResult -> TestRunResult
      updateFlakinessMessage trr = case mMsg of
        Nothing -> trr
        Just msg -> trr {testRunResultFlakinessMessage = Just msg}
      go i
        | i <= 1 = runFunc
        | otherwise = do
          result <- runFunc
          case testRunResultStatus result of
            TestPassed -> pure result
            TestFailed -> updateRetriesResult <$> go (pred i)
        where
          updateRetriesResult :: TestRunResult -> TestRunResult
          updateRetriesResult trr =
            trr
              { testRunResultRetries =
                  case testRunResultRetries trr of
                    Nothing -> Just 1
                    Just r -> Just (succ r)
              }
  where
    runFunc :: IO TestRunResult
    runFunc = testDefVal td progressReporter (\f -> f l ())
