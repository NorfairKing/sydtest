{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.AMQPSpec (spec) where

import Network.AMQP
import Test.Syd
import Test.Syd.AMQP

spec :: Spec
spec = do
  rabbitMQSpec $ do
    describe "rabbitMQSpec" $
      it "can run the server and clean up nicely" $ do
        pure () :: IO ()
  amqpSpec $ do
    describe "amqpSpec" $
      it "can write and read a message" $ \conn -> do
        chan <- openChannel conn

        -- declare a queue, exchange and binding
        declareQueue chan newQueue {queueName = "myQueue"}
        declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
        bindQueue chan "myQueue" "myExchange" "myKey"

        -- publish a message to our new exchange
        publishMsg
          chan
          "myExchange"
          "myKey"
          newMsg
            { msgBody = "hello world",
              msgDeliveryMode = Just Persistent
            }

        mMesg <- getMsg chan Ack "myQueue"
        case mMesg of
          Nothing -> expectationFailure "Should have received a message"
          Just (_, _) -> pure ()
