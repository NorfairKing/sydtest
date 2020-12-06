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
      let t :: HList '[Int] -> () -> IO ()
          t (HCons i HNil) () = i `shouldBe` 1
      it' "reads 1" t
      it' "reads 1" t

  describe "beforeAll_" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment :: IO ()
        increment = atomically $ modifyTVar var (+ 1)
    beforeAll_ increment $ do
      let t :: IO ()
          t = do
            i <- readTVarIO var
            i `shouldBe` 1
      it' "reads 1" t
      it' "reads 1" t

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
        let t :: HList '[Int, Int] -> () -> IO ()
            t (HCons i (HCons j HNil)) () = do
              i `shouldBe` 3
              j `shouldBe` 1
        it' "reads 3" t
        it' "reads 3" t

  describe "afterAll" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let readAndIncrement :: IO Int
        readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
        addExtra :: Int -> IO ()
        addExtra i = atomically $ modifyTVar var (+ i)
    beforeAll readAndIncrement $
      afterAll addExtra $ do
        let t :: HList '[Int] -> () -> IO ()
            t (HCons i HNil) () = i `shouldBe` 1
        it' "reads 1" t
        it' "reads 1" t

  describe "afterAll'" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let readAndIncrement :: IO Int
        readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
        addExtra :: HList '[Int] -> IO ()
        addExtra (HCons i HNil) = atomically $ modifyTVar var (+ i)
    beforeAll readAndIncrement $
      afterAll' addExtra $ do
        let t :: HList '[Int] -> () -> IO ()
            t (HCons i HNil) () = i `shouldBe` 1
        it' "reads 1" t
        it' "reads 1" t

  describe "afterAll_" $ do
    var <- liftIO $ newTVarIO (0 :: Int)
    let increment :: IO ()
        increment = atomically $ modifyTVar var (+ 1)
    afterAll_ increment $ do
      let t :: IO ()
          t = do
            i <- readTVarIO var
            i `shouldBe` 0
      it' "reads 0" t
      it' "reads 0" t

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
      let t :: HList '[Int] -> () -> IO ()
          t (HCons i HNil) () = i `shouldBe` 1
      it' "reads 1" t
      it' "reads 1" t

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
        let t :: HList '[Int, Int] -> () -> IO ()
            t (HCons i (HCons j HNil)) () = do
              i `shouldBe` 3
              j `shouldBe` 1
        it' "reads correctly" t
        it' "reads correctly" t

-- describe "aroundAll" $ do
--   var <- liftIO $ newTVarIO (0 :: Int)
--   let increment = atomically $ modifyTVar var succ
--       readAndIncrement = atomically $ stateTVar var $ \i -> (i, i + 1)
--       aroundAllWithFunc :: (Int -> IO ()) -> Int -> IO ()
--       aroundAllWithFunc intFunc j = do
--         i <- readAndIncrement
--         intFunc $ i + j
--         increment
--       aroundAllFunc :: (HList Int '[] -> IO ()) -> IO ()
--       aroundAllFunc intFunc = do
--         i <- readAndIncrement
--         intFunc (HLast i)
--         increment
--       aroundAllFunc_ :: IO () -> IO ()
--       aroundAllFunc_ func = do
--         increment
--         func
--         increment
--   aroundAll aroundAllFunc $
--     aroundAllWith aroundAllWithFunc $
--       aroundAll_ aroundAllFunc_ $ do
--         pure () :: TestDefM (HList Int '[Int]) () ()
--         it "reads 1" $ \(HCons i (HLast j)) () -> do
--           i `shouldBe` 1
--           j `shouldBe` 1
--         it "reads 1" $ \(HCons i (HLast j)) () -> do
--           i `shouldBe` 1
--           j `shouldBe` 1
--         it "reads 1" $ \(HCons i (HLast j)) () -> do
--           i `shouldBe` 1
--           j `shouldBe` 1
