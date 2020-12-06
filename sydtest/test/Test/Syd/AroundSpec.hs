module Test.Syd.AroundSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Test.Syd

spec :: Spec
spec = sequential $ do
  describe "before" $ do
    var <- liftIO $ newTVarIO (1 :: Int)
    let readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
    before readAndIncrement $ do
      it "reads 2" $ \i ->
        i `shouldBe` 2
      it "reads 4" $ \i ->
        i `shouldBe` 3
      it "reads 6" $ \i ->
        i `shouldBe` 4

  describe "before_" $ do
    var <- liftIO $ newTVarIO (1 :: Int)
    let increment = atomically $ modifyTVar var succ
    before_ increment $ do
      it "reads 2" $ do
        i <- readTVarIO var
        i `shouldBe` 2
      it "reads 4" $ do
        i <- readTVarIO var
        i `shouldBe` 3
      it "reads 6" $ do
        i <- readTVarIO var
        i `shouldBe` 4

  describe "after" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment = atomically $ modifyTVar var succ
    after (\() -> increment) $ do
      it "reads 0" $ do
        i <- readTVarIO var
        i `shouldBe` 0
      it "reads 2" $ do
        i <- readTVarIO var
        i `shouldBe` 1
      it "reads 4" $ do
        i <- readTVarIO var
        i `shouldBe` 2

  describe "after_" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment = atomically $ modifyTVar var succ
    after_ increment $ do
      it "reads 0" $ do
        i <- readTVarIO var
        i `shouldBe` 0
      it "reads 2" $ do
        i <- readTVarIO var
        i `shouldBe` 1
      it "reads 4" $ do
        i <- readTVarIO var
        i `shouldBe` 2

  describe "around" $ do
    var <- liftIO $ newTVarIO (1 :: Int)
    let increment = atomically $ modifyTVar var succ
        readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
        aroundFunc :: (Int -> IO ()) -> IO ()
        aroundFunc intFunc = do
          i <- readAndIncrement
          intFunc i
          increment
    around aroundFunc $ do
      it "reads 2" $ \i ->
        i `shouldBe` 2
      it "reads 4" $ \i ->
        i `shouldBe` 4
      it "reads 6" $ \i ->
        i `shouldBe` 6

  describe "around_" $ do
    var <- liftIO $ newTVarIO (1 :: Int)
    let increment = atomically $ modifyTVar var succ
        aroundFunc_ :: IO () -> IO ()
        aroundFunc_ func = do
          increment
          func
          increment
    around_ aroundFunc_ $ do
      it "reads 2" $ do
        i <- readTVarIO var
        i `shouldBe` 2
      it "reads 4" $ do
        i <- readTVarIO var
        i `shouldBe` 4
      it "reads 6" $ do
        i <- readTVarIO var
        i `shouldBe` 6

  describe "aroundWith" $ do
    var <- liftIO $ newTVarIO (1 :: Int)
    let increment = atomically $ modifyTVar var succ
        readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
        aroundWithFunc :: (Int -> IO ()) -> () -> IO ()
        aroundWithFunc intFunc () = do
          i <- readAndIncrement
          intFunc i
          increment
    aroundWith aroundWithFunc $ do
      it "reads 2" $ \i ->
        i `shouldBe` 2
      it "reads 4" $ \i ->
        i `shouldBe` 4
      it "reads 6" $ \i ->
        i `shouldBe` 6
