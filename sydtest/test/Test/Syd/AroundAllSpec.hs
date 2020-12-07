{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.AroundAllSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Test.Syd

spec :: Spec
spec = sequential $ do
  describe "beforeAll" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let readAndIncrement :: IO Int
        readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
    beforeAll readAndIncrement $ do
      let t :: Int -> () -> IO ()
          t i () = i `shouldBe` 1
      itWithOuter "reads 1" t
      itWithOuter "reads 1" t

  describe "beforeAll_" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment :: IO ()
        increment = atomically $ modifyTVar var (+ 1)
    beforeAll_ increment $ do
      let t :: IO ()
          t = do
            i <- readTVarIO var
            i `shouldBe` 1
      it "reads 1" t
      it "reads 1" t

  describe "beforeAllWith" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let readAndIncrement :: IO Int
        readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
    let increment :: IO ()
        increment = atomically $ modifyTVar var (+ 1)
    let incrementBeforeAndAfterWith :: Int -> IO Int
        incrementBeforeAndAfterWith j = do
          i <- readAndIncrement
          increment
          pure (i + j)
    beforeAll readAndIncrement $ do
      beforeAllWith incrementBeforeAndAfterWith $ do
        let t :: Int -> () -> IO ()
            t i () = i `shouldBe` 3
        itWithOuter "reads 3" t
        itWithOuter "reads 3" t

  describe "afterAll" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let readAndIncrement :: IO Int
        readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
        addExtra :: Int -> IO ()
        addExtra i = atomically $ modifyTVar var (+ i)
    beforeAll readAndIncrement $
      afterAll addExtra $ do
        let t :: Int -> () -> IO ()
            t i () = i `shouldBe` 1
        itWithOuter "reads 1" t
        itWithOuter "reads 1" t

  describe "afterAll'" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let readAndIncrement :: IO Int
        readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
        addExtra :: HList '[Int] -> IO ()
        addExtra (HCons i HNil) = atomically $ modifyTVar var (+ i)
    beforeAll readAndIncrement $
      afterAll' addExtra $ do
        let t :: Int -> () -> IO ()
            t i () = i `shouldBe` 1
        itWithOuter "reads 1" t
        itWithOuter "reads 1" t

  describe "afterAll_" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment :: IO ()
        increment = atomically $ modifyTVar var (+ 1)
    afterAll_ increment $ do
      let t :: IO ()
          t = do
            i <- readTVarIO var
            i `shouldBe` 0
      it "reads 0" t
      it "reads 0" t

  describe "aroundAll" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let readAndIncrement :: IO Int
        readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
    let increment :: IO ()
        increment = atomically $ modifyTVar var (+ 1)
    let incrementBeforeAndAfter :: (Int -> IO ()) -> IO ()
        incrementBeforeAndAfter func = do
          i <- readAndIncrement
          func i
          increment
    aroundAll incrementBeforeAndAfter $ do
      let t :: Int -> () -> IO ()
          t i () = i `shouldBe` 1
      itWithOuter "reads 1" t
      itWithOuter "reads 1" t

  describe "aroundAll_" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment :: IO ()
        increment = atomically $ modifyTVar var (+ 1)
    let incrementBeforeAndAfter :: IO () -> IO ()
        incrementBeforeAndAfter func = do
          increment
          func
          increment
    aroundAll_ incrementBeforeAndAfter $ do
      let t :: IO ()
          t = do
            i <- readTVarIO var
            i `shouldBe` 1
      it "reads 1" t
      it "reads 1" t

  describe "aroundAllWith" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let readAndIncrement :: IO Int
        readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
    let increment :: IO ()
        increment = atomically $ modifyTVar var (+ 1)
    let incrementBeforeAndAfter :: (Int -> IO ()) -> IO ()
        incrementBeforeAndAfter func = do
          i <- readAndIncrement
          func i
          increment
    let incrementBeforeAndAfterWith :: (Int -> IO ()) -> Int -> IO ()
        incrementBeforeAndAfterWith func j = do
          i <- readAndIncrement
          func (i + j)
          increment
    aroundAll incrementBeforeAndAfter $
      aroundAllWith incrementBeforeAndAfterWith $ do
        let t :: Int -> () -> IO ()
            t i () = i `shouldBe` 3
        itWithOuter "reads correctly" t
        itWithOuter "reads correctly" t
