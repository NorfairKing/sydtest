module AroundSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Test.Syd

spec :: Spec
spec = sequential $ do
  describe "before" $ do
    var <- liftIO $ newTVarIO (1 :: Int)
    let readAndIncrement = atomically $ stateTVar var $ \i -> (i, i + 1)
    before_ (() <$ readAndIncrement) $
      before readAndIncrement $ do
        it "reads 2" $ \i ->
          i `shouldBe` 2
        it "reads 4" $ \i ->
          i `shouldBe` 4
        it "reads 6" $ \i ->
          i `shouldBe` 6

  describe "after" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment = atomically $ modifyTVar var succ
    after_ (() <$ increment) $
      after (\() -> increment) $ do
        it "reads 0" $ do
          i <- readTVarIO var
          i `shouldBe` 0
        it "reads 2" $ do
          i <- readTVarIO var
          i `shouldBe` 2
        it "reads 4" $ do
          i <- readTVarIO var
          i `shouldBe` 4

  describe "around" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment = atomically $ modifyTVar var succ
        readAndIncrement = atomically $ stateTVar var $ \i -> (i, i + 1)
        aroundWithFunc :: (Int -> IO ()) -> Int -> IO ()
        aroundWithFunc intFunc j = do
          i <- readAndIncrement
          intFunc $ i + j
          increment
        aroundFunc :: (Int -> IO ()) -> IO ()
        aroundFunc intFunc = do
          i <- readAndIncrement
          intFunc i
          increment
        aroundFunc_ :: IO () -> IO ()
        aroundFunc_ func = do
          increment
          func
          increment
    around_ aroundFunc_ $
      around aroundFunc $
        aroundWith aroundWithFunc $ do
          it "reads 2" $ \i ->
            i `shouldBe` 3
          it "reads 4" $ \i ->
            i `shouldBe` 15
          it "reads 6" $ \i ->
            i `shouldBe` 27
