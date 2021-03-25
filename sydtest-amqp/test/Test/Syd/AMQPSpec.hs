{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.AMQPSpec (spec) where

import Network.AMQP
import Test.Syd
import Test.Syd.AMQP

spec :: Spec
spec = do
  amqpSpec $
    doNotRandomiseExecutionOrder $ do
      describe "amqpSpec" $ do
        it "can write and read a message" $ \conn -> do
          chan <- openChannel conn

          -- declare a queue, exchange and binding
          _ <- declareQueue chan newQueue {queueName = "myQueue"}
          declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
          bindQueue chan "myQueue" "myExchange" "myKey"

          -- publish a message to our new exchange
          let body = "hello world"
          _ <-
            publishMsg
              chan
              "myExchange"
              "myKey"
              newMsg
                { msgBody = body,
                  msgDeliveryMode = Just Persistent
                }

          mMesg <- getMsg chan Ack "myQueue"
          case mMesg of
            Nothing -> expectationFailure "Should have received a message"
            Just (m, e) -> do
              msgBody m `shouldBe` body
              ackEnv e
        it "can write a message (to make sure the next test cannot read it)" $ \conn -> do
          chan <- openChannel conn

          -- declare a queue, exchange and binding
          _ <- declareQueue chan newQueue {queueName = "myQueue"}
          declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
          bindQueue chan "myQueue" "myExchange" "myKey"

          -- publish a message to our new exchange
          let body = "hello world"
          _ <-
            publishMsg
              chan
              "myExchange"
              "myKey"
              newMsg
                { msgBody = body,
                  msgDeliveryMode = Just Persistent
                }
          pure ()

        it "cannot read a message if none have been published" $ \conn -> do
          chan <- openChannel conn

          -- declare a queue, exchange and binding
          _ <- declareQueue chan newQueue {queueName = "myQueue"}
          declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
          bindQueue chan "myQueue" "myExchange" "myKey"

          -- Don't publish anything
          mMesg <- getMsg chan Ack "myQueue"
          case mMesg of
            Nothing -> pure ()
            Just (m, _) ->
              expectationFailure $
                unlines
                  [ "Should not have been able to read any message, but read this one:",
                    show m
                  ]
