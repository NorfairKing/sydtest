{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module AroundAllSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Test.Syd

spec :: Spec
spec = sequential $
  describe "beforeAll" $ do
    var <- liftIO $ newTVarIO (1 :: Int)
    let readAndIncrement :: IO Int
        readAndIncrement = atomically $ stateTVar var $ \i -> (i, i + 1)
    beforeAll readAndIncrement $ do
      let t :: HList '[Int] -> () -> IO ()
          t (HCons i HNil) () = i `shouldBe` 1
      it' "reads 2" t
      it' "reads 2" t

--  describe "beforeAll" $ do
--    var <- liftIO $ newTVarIO (1 :: Int)
--    let readAndIncrement = atomically $ stateTVar var $ \i -> (i, i + 1)
--    beforeAll_ (() <$ readAndIncrement) $
--      beforeAll readAndIncrement $ do
--        it "reads 2" $ \i () ->
--          i `shouldBe` 2
--        it "reads 2" $ \i () ->
--          i `shouldBe` 2
--        it "reads 2" $ \i () ->
--          i `shouldBe` 2
--
--  describe "afterAll" $ do
--    var <- liftIO $ newTVarIO (0 :: Int)
--    let increment = atomically $ modifyTVar var succ
--    afterAll_ (() <$ increment) $
--      afterAll (\() -> increment) $ do
--        it "reads 0" $ do
--          i <- readTVarIO var
--          i `shouldBe` 0
--        it "reads 0" $ do
--          i <- readTVarIO var
--          i `shouldBe` 0
--        it "reads 0" $ do
--          i <- readTVarIO var
--          i `shouldBe` 0
--
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
