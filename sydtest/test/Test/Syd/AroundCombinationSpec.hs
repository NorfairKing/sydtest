{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.AroundCombinationSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Test.Syd

spec :: Spec
spec = sequential $
  doNotRandomiseExecutionOrder $ do
    describe "aroundAll + aroundAllWith + around + aroundWith'" $ do
      outerOuterVar <- liftIO $ newTVarIO (0 :: Word64)
      outerVar <- liftIO $ newTVarIO (0 :: Word32)
      innerVar <- liftIO $ newTVarIO (0 :: Int64)
      innerInnerVar <- liftIO $ newTVarIO (0 :: Int32)
      let increment :: Enum i => TVar i -> IO ()
          increment var = atomically $ modifyTVar var succ
      let aroundAllFunc :: (Word64 -> IO s) -> IO s
          aroundAllFunc func = do
            increment outerOuterVar
            w <- readTVarIO outerOuterVar
            s <- func w
            increment outerOuterVar
            pure s
          aroundAllWithFunc :: (Word32 -> IO s) -> Word64 -> IO s
          aroundAllWithFunc func ow = do
            increment outerVar
            w <- readTVarIO outerVar
            s <- func $ fromIntegral ow + w
            increment outerVar
            pure s
          aroundFunc :: (Int64 -> IO s) -> IO s
          aroundFunc func = do
            increment innerVar
            i <- readTVarIO innerVar
            s <- func i
            increment innerVar
            pure s
          aroundWithFunc :: (Int32 -> IO s) -> Int64 -> IO s
          aroundWithFunc func oi = do
            increment innerInnerVar
            i <- readTVarIO innerInnerVar
            s <- func $ fromIntegral oi + i
            increment innerInnerVar
            pure s
          aroundWith'Func :: (Word32 -> Int16 -> IO s) -> Word32 -> Int32 -> IO s
          aroundWith'Func func iw ii = do
            increment innerInnerVar
            i <- readTVarIO innerInnerVar
            s <- func iw $ fromIntegral $ i + fromIntegral iw + ii
            increment innerInnerVar
            pure s
      aroundAll aroundAllFunc $
        aroundAllWith aroundAllWithFunc $ do
          -- pure () :: TestDefM '[Word32, Word64] () ()
          around aroundFunc $ do
            aroundWith aroundWithFunc $
              aroundWith' aroundWith'Func $ do
                itWithBoth "reads correctly" $
                  \i k -> (i, k) `shouldBe` (3, 3)
                itWithBoth "reads correctly" $
                  \i k -> (i, k) `shouldBe` (3, 7)
