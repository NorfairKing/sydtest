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
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Text as T
import Rainbow
import Test.Syd.Def.SetupFunc
import Test.Syd.HList
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Runner.Wrappers
import Test.Syd.SpecDef
import Test.Syd.SpecForest

runSpecForestSynchronously :: Bool -> TestForest '[] () -> IO ResultForest
runSpecForestSynchronously failFast = fmap extractNext . goForest HNil
  where
    goForest :: HList a -> TestForest a () -> IO (Next ResultForest)
    goForest _ [] = pure (Continue [])
    goForest l (tt : rest) = do
      nrt <- goTree l tt
      case nrt of
        Continue rt -> do
          nf <- goForest l rest
          pure $ (rt :) <$> nf
        Stop rt -> pure $ Stop [rt]
    goTree :: forall a. HList a -> TestTree a () -> IO (Next ResultTree)
    goTree l = \case
      DefSpecifyNode t td () -> do
        let runFunc = testDefVal td (\f -> f l ())
        result <- timeItT runFunc
        let td' = td {testDefVal = result}
        let r = failFastNext failFast td'
        pure $ SpecifyNode t <$> r
      DefPendingNode t mr -> pure $ Continue $ PendingNode t mr
      DefDescribeNode t sdf -> fmap (DescribeNode t) <$> goForest l sdf
      DefWrapNode func sdf -> fmap SubForestNode <$> applySimpleWrapper'' func (goForest l sdf)
      DefBeforeAllNode func sdf -> do
        fmap SubForestNode
          <$> ( do
                  b <- func
                  goForest (HCons b l) sdf
              )
      DefAroundAllNode func sdf ->
        fmap SubForestNode <$> unSetupFunc func (\b -> goForest (HCons b l) sdf) ()
      DefAroundAllWithNode func sdf ->
        let HCons x _ = l
         in fmap SubForestNode <$> unSetupFunc func (\b -> goForest (HCons b l) sdf) x
      DefAfterAllNode func sdf -> fmap SubForestNode <$> (goForest l sdf `finally` func l)
      DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest l sdf -- Ignore, it's synchronous anyway
      DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest l sdf

runSpecForestInterleavedWithOutputSynchronously :: Maybe Bool -> Bool -> TestForest '[] () -> IO (Timed ResultForest)
runSpecForestInterleavedWithOutputSynchronously mColour failFast testForest = do
  byteStringMaker <- case mColour of
    Just False -> pure toByteStringsColors0
    Just True -> pure toByteStringsColors256
    Nothing -> liftIO byteStringMakerFromEnvironment
  let outputLine :: [Chunk] -> IO ()
      outputLine lineChunks = do
        let bss = chunksToByteStrings byteStringMaker lineChunks
        liftIO $ do
          mapM_ SB.putStr bss
          SB8.putStrLn ""
      treeWidth :: Int
      treeWidth = specForestWidth testForest
  let pad :: Int -> [Chunk] -> [Chunk]
      pad level = (chunk (T.pack (replicate (paddingSize * level) ' ')) :)
      goForest :: Int -> HList a -> TestForest a () -> IO (Next ResultForest)
      goForest _ _ [] = pure (Continue [])
      goForest level l (tt : rest) = do
        nrt <- goTree level l tt
        case nrt of
          Continue rt -> do
            nf <- goForest level l rest
            pure $ (rt :) <$> nf
          Stop rt -> pure $ Stop [rt]
      goTree :: Int -> HList a -> TestTree a () -> IO (Next ResultTree)
      goTree level a = \case
        DefSpecifyNode t td () -> do
          let runFunc = testDefVal td (\f -> f a ())
          result <- timeItT runFunc
          let td' = td {testDefVal = result}
          mapM_ (outputLine . pad level) $ outputSpecifyLines level treeWidth t td'
          let r = failFastNext failFast td'
          pure $ SpecifyNode t <$> r
        DefPendingNode t mr -> do
          mapM_ (outputLine . pad level) $ outputPendingLines t mr
          pure $ Continue $ PendingNode t mr
        DefDescribeNode t sf -> do
          outputLine $ pad level $ outputDescribeLine t
          fmap (DescribeNode t) <$> goForest (succ level) a sf
        DefWrapNode func sdf -> fmap SubForestNode <$> applySimpleWrapper'' func (goForest level a sdf)
        DefBeforeAllNode func sdf ->
          fmap SubForestNode
            <$> ( do
                    b <- func
                    goForest level (HCons b a) sdf
                )
        DefAroundAllNode func sdf ->
          fmap SubForestNode <$> unSetupFunc func (\b -> goForest level (HCons b a) sdf) ()
        DefAroundAllWithNode func sdf ->
          let HCons x _ = a
           in fmap SubForestNode <$> unSetupFunc func (\b -> goForest level (HCons b a) sdf) x
        DefAfterAllNode func sdf -> fmap SubForestNode <$> (goForest level a sdf `finally` func a)
        DefParallelismNode _ sdf -> fmap SubForestNode <$> goForest level a sdf -- Ignore, it's synchronous anyway
        DefRandomisationNode _ sdf -> fmap SubForestNode <$> goForest level a sdf
  mapM_ outputLine outputTestsHeader
  resultForest <- timeItT $ extractNext <$> goForest 0 HNil testForest
  outputLine [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading (timedValue resultForest)
  outputLine [chunk " "]
  mapM_ outputLine $ outputStats (computeTestSuiteStats <$> resultForest)
  outputLine [chunk " "]

  pure resultForest
