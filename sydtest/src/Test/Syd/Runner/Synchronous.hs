{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines how to run a test suite
module Test.Syd.Runner.Synchronous where

import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.Text as T
import Rainbow
import Test.Syd.HList
import Test.Syd.Output
import Test.Syd.Runner.Wrappers
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import UnliftIO

runSpecForestSynchronously :: TestForest '[] () -> IO ResultForest
runSpecForestSynchronously = goForest HNil
  where
    goForest :: HList a -> TestForest a () -> IO ResultForest
    goForest l = mapM (goTree l)
    goTree :: forall a. HList a -> TestTree a () -> IO ResultTree
    goTree l = \case
      DefDescribeNode t sdf -> DescribeNode t <$> goForest l sdf
      DefSpecifyNode t td () -> do
        let runFunc = testDefVal td (\f -> f l ())
        result <- runFunc
        let td' = td {testDefVal = result}
        pure $ SpecifyNode t td'
      DefWrapNode func sdf -> SubForestNode <$> applySimpleWrapper'' func (goForest l sdf)
      DefBeforeAllNode func sdf -> do
        SubForestNode
          <$> ( do
                  b <- func
                  goForest (HCons b l) sdf
              )
      DefBeforeAllWithNode func sdf -> do
        SubForestNode
          <$> ( do
                  let HCons x _ = l
                  b <- func x
                  goForest (HCons b l) sdf
              )
      DefAroundAllNode func sdf ->
        SubForestNode <$> applySimpleWrapper' func (\b -> goForest (HCons b l) sdf)
      DefAroundAllWithNode func sdf ->
        let HCons x _ = l
         in SubForestNode <$> applySimpleWrapper func (\b -> goForest (HCons b l) sdf) x
      DefAfterAllNode func sdf -> SubForestNode <$> (goForest l sdf `finally` func l)
      DefParallelismNode _ sdf -> SubForestNode <$> goForest l sdf -- Ignore, it's synchronous anyway

runSpecForestInterleavedWithOutputSynchronously :: TestForest '[] () -> IO ResultForest
runSpecForestInterleavedWithOutputSynchronously testForest = do
  byteStringMaker <- liftIO byteStringMakerFromEnvironment
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
      goTree :: Int -> HList a -> TestTree a () -> IO ResultTree
      goTree level a = \case
        DefDescribeNode t sf -> do
          outputLine $ pad level $ outputDescribeLine t
          DescribeNode t <$> goForest (succ level) a sf
        DefSpecifyNode t td () -> do
          let runFunc = testDefVal td (\f -> f a ())
          result <- runFunc
          let td' = td {testDefVal = result}
          mapM_ (outputLine . pad level) $ outputSpecifyLines level treeWidth t td'
          pure $ SpecifyNode t td'
        DefWrapNode func sdf -> SubForestNode <$> applySimpleWrapper'' func (goForest level a sdf)
        DefBeforeAllNode func sdf ->
          SubForestNode
            <$> ( do
                    b <- func
                    goForest level (HCons b a) sdf
                )
        DefBeforeAllWithNode func sdf ->
          SubForestNode
            <$> ( do
                    let HCons x _ = a
                    b <- func x
                    goForest level (HCons b a) sdf
                )
        DefAroundAllNode func sdf ->
          SubForestNode <$> applySimpleWrapper' func (\b -> goForest level (HCons b a) sdf)
        DefAroundAllWithNode func sdf ->
          let HCons x _ = a
           in SubForestNode <$> applySimpleWrapper func (\b -> goForest level (HCons b a) sdf) x
        DefAfterAllNode func sdf -> SubForestNode <$> (goForest level a sdf `finally` func a)
        DefParallelismNode _ sdf -> SubForestNode <$> goForest level a sdf -- Ignore, it's synchronous anyway
      goForest :: Int -> HList a -> TestForest a () -> IO ResultForest
      goForest level a = mapM (goTree level a)
  mapM_ outputLine outputTestsHeader
  resultForest <- goForest 0 HNil testForest
  outputLine $ [chunk " "]
  mapM_ outputLine $ outputFailuresWithHeading resultForest
  pure resultForest
