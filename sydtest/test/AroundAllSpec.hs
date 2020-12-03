module AroundAllSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Test.Syd

spec :: Spec
spec = sequential $ do
  describe "beforeAll" $ do
    var <- liftIO $ newTVarIO (1 :: Int)
    let readAndIncrement = atomically $ stateTVar var $ \i -> (i, i + 1)
    beforeAll_ (() <$ readAndIncrement) $
      beforeAll readAndIncrement $ do
        it "reads 2" $ \i () ->
          i `shouldBe` 2
        it "reads 2" $ \i () ->
          i `shouldBe` 2
        it "reads 2" $ \i () ->
          i `shouldBe` 2

  describe "afterAll" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment = atomically $ modifyTVar var succ
    afterAll_ (() <$ increment) $
      afterAll (\() -> increment) $ do
        it "reads 0" $ do
          i <- readTVarIO var
          i `shouldBe` 0
        it "reads 0" $ do
          i <- readTVarIO var
          i `shouldBe` 0
        it "reads 0" $ do
          i <- readTVarIO var
          i `shouldBe` 0

  describe "aroundAll" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment = atomically $ modifyTVar var succ
        readAndIncrement = atomically $ stateTVar var $ \i -> (i, i + 1)
        aroundAllWithFunc :: (Int -> IO ()) -> Int -> IO ()
        aroundAllWithFunc intFunc j = do
          i <- readAndIncrement
          intFunc $ i + j
          increment
        aroundAllFunc :: (Int -> IO ()) -> IO ()
        aroundAllFunc intFunc = do
          i <- readAndIncrement
          intFunc i
          increment
        aroundAllFunc_ :: IO () -> IO ()
        aroundAllFunc_ func = do
          increment
          func
          increment
    aroundAll aroundAllFunc $
      aroundAllWith aroundAllWithFunc $
        aroundAll_ aroundAllFunc_ $ do
          it "reads 1" $ \i () ->
            i `shouldBe` 1
          it "reads 1" $ \i () ->
            i `shouldBe` 1
          it "reads 1" $ \i () ->
            i `shouldBe` 1
