{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.RedisSpec (spec) where

import Database.Redis as Redis
import Test.Syd
import Test.Syd.Redis

spec :: Spec
spec = do
  describe "redisServerSpec" $
    redisServerSpec $
      it "sets up and tears down the redis server nicely" $ do
        pure () :: IO ()
  describe "redisSpec" $
    redisSpec $ do
      it "sets up and tears down a redis connection nicely" $ \conn -> do
        runRedis conn $ do
          errOrStatus <- Redis.set "hello" "world"
          liftIO $ case errOrStatus of
            Left err -> expectationFailure $ show err
            Right status -> status `shouldBe` Ok
          errOrReply <- Redis.get "hello"
          liftIO $ case errOrReply of
            Left err -> expectationFailure $ show err
            Right val -> val `shouldBe` Just "world"
      doNotRandomiseExecutionOrder $
        describe "shared state" $ do
          it "can write a message" $ \conn -> runRedis conn $ do
            errOrStatus <- Redis.set "hello" "world"
            liftIO $ case errOrStatus of
              Left err -> expectationFailure $ show err
              Right status -> status `shouldBe` Ok
          it "cannot read a message that has not been written yet" $ \conn -> runRedis conn $ do
            errOrReply <- Redis.get "hello"
            liftIO $ case errOrReply of
              Left err -> expectationFailure $ show err
              Right Nothing -> pure ()
              Right (Just val) -> expectationFailure $ "Should not have been able to read anything, but got: " <> show val
