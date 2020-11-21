{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd
  ( module Test.Syd,
    module Test.Syd.Run,
    module Test.Syd.SpecForest,
    module Test.Syd.Expectation,
    module Test.Syd.Output,
    module Test.Syd.Silence,
    module Test.Syd.Def,
  )
where

import Control.Concurrent.QSem
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Text as T
import Rainbow
import System.Exit
import Test.QuickCheck.IO ()
import Test.Syd.Def
import Test.Syd.Expectation
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Silence
import Test.Syd.SpecForest
import UnliftIO

sydTest :: Spec -> IO ()
sydTest spec = do
  resultForest <- sydTestResult spec
  when (shouldExitFail resultForest) (exitWith (ExitFailure 1))

sydTestResult :: Spec -> IO ResultForest
sydTestResult spec = do
  specForest <- execTestDefM spec
  runSpecForestInterleavedWithOutputSynchronously specForest

runSpecForestSynchronously :: TestForest () () -> IO ResultForest
runSpecForestSynchronously = goForest ()
  where
    goForest :: a -> TestForest a () -> IO ResultForest
    goForest a = mapM (goTree a)
    goTree :: forall a. a -> TestTree a () -> IO ResultTree
    goTree a = \case
      DefDescribeNode t sdf -> DescribeNode t <$> goForest a sdf
      DefSpecifyNode t td () -> do
        let runFunc = testDefVal td (\f -> f a ())
        result <- runFunc
        let td' = td {testDefVal = result}
        pure $ SpecifyNode t td'
      DefAroundAllNode func sdf -> AroundAllNode <$> applySimpleWrapper func (\b -> goForest b sdf) a

applySimpleWrapper ::
  MonadIO m =>
  ((a -> m ()) -> (b -> m ())) ->
  (a -> m r) ->
  (b -> m r)
applySimpleWrapper takeTakeA takeA b = do
  var <- liftIO $ newEmptyMVar
  takeTakeA
    ( \a -> do
        r <- takeA a
        liftIO $ putMVar var r
    )
    b
  liftIO $ readMVar var

runSpecForestInterleavedWithOutputSynchronously :: TestForest () () -> IO ResultForest
runSpecForestInterleavedWithOutputSynchronously testForest = do
  byteStringMaker <- liftIO byteStringMakerFromEnvironment
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = do
        let bss = chunksToByteStrings byteStringMaker lineChunks
        liftIO $ do
          mapM_ SB.putStr bss
          SB8.putStrLn ""
  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.replicate (level * 2) " ") :)
      goTree :: Int -> a -> TestTree a () -> IO ResultTree
      goTree level a = \case
        DefDescribeNode t sf -> do
          outputLine $ pad level $ outputDescribeLine t
          DescribeNode t <$> goForest (succ level) a sf
        DefSpecifyNode t td () -> do
          let runFunc = testDefVal td (\f -> f a ())
          result <- runFunc
          let td' = td {testDefVal = result}
          mapM_ (outputLine . pad level) $ outputSpecifyLines t td'
          pure $ SpecifyNode t td'
        DefAroundAllNode func sdf -> AroundAllNode <$> applySimpleWrapper func (\b -> goForest level b sdf) a
      goForest :: Int -> a -> TestForest a () -> IO ResultForest
      goForest level a = mapM (goTree level a)
  mapM_ outputLine outputTestsHeader
  resultForest <- goForest 0 () testForest
  outputLine $ [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading resultForest
  pure resultForest

-- This fails miserably when silencing is used.
runSpecForestInterleavedWithOutputAsynchronously :: Int -> TestForest () () -> IO ResultForest
runSpecForestInterleavedWithOutputAsynchronously nbThreads testForest = do
  handleForest <- makeHandleForest testForest
  let runRunner = runner nbThreads handleForest
      runPrinter = liftIO $ printer handleForest
  ((), resultForest) <- concurrently runRunner runPrinter
  pure resultForest

type HandleForest a b = SpecDefForest a b (MVar TestRunResult)

type HandleTree a b = SpecDefTree a b (MVar TestRunResult)

makeHandleForest :: TestForest a b -> IO (HandleForest a b)
makeHandleForest = traverse $ traverse $ \() -> do
  var <- newEmptyMVar
  pure var

runner :: Int -> HandleForest () () -> IO ()
runner nbThreads handleForest = do
  sem <- liftIO $ newQSem nbThreads
  let goForest :: a -> HandleForest a () -> IO ()
      goForest a = mapM_ (goTree a)
      goTree :: a -> HandleTree a () -> IO ()
      goTree a = \case
        DefDescribeNode _ sdf -> goForest a sdf
        DefSpecifyNode _ td var -> do
          liftIO $ waitQSem sem
          let job :: IO ()
              job = do
                result <- testDefVal td (\f -> f a ())
                putMVar var result
                liftIO $ signalQSem sem
          jobAsync <- async job
          link jobAsync
        DefAroundAllNode func sdf -> applySimpleWrapper func (\b -> goForest b sdf) a
  goForest () handleForest

printer :: HandleForest () () -> IO ResultForest
printer handleForest = do
  byteStringMaker <- liftIO byteStringMakerFromEnvironment
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = do
        let bss = chunksToByteStrings byteStringMaker lineChunks
        mapM_ SB.putStr bss
        SB8.putStrLn ""
  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.replicate (level * 2) " ") :)
      goTree :: Int -> HandleTree a b -> IO ResultTree
      goTree level = \case
        DefDescribeNode t sf -> do
          outputLine $ pad level $ outputDescribeLine t
          DescribeNode t <$> goForest (succ level) sf
        DefSpecifyNode t td var -> do
          result <- takeMVar var
          let td' = td {testDefVal = result}
          mapM_ (outputLine . pad level) $ outputSpecifyLines t td'
          pure $ SpecifyNode t td'
        DefAroundAllNode _ sdf -> AroundAllNode <$> goForest level sdf
      goForest :: Int -> HandleForest a b -> IO ResultForest
      goForest level = mapM (goTree level)
  mapM_ outputLine $ outputTestsHeader
  resultForest <- goForest 0 handleForest
  outputLine $ [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading resultForest
  pure resultForest

shouldExitFail :: ResultForest -> Bool
shouldExitFail = any (any ((== TestFailed) . testRunResultStatus . testDefVal))
