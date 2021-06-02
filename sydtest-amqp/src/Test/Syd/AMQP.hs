{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.AMQP
  ( amqpSpec,
    amqpConnectionSetupFunc,
  )
where

import Network.AMQP as AMQP
import Test.Syd
import Test.Syd.RabbitMQ

-- | Run a rabbitmq server around a group of test, and provide a clean connection to each individual test
--
-- Example usage
--
-- > spec :: Spec
-- > spec =
-- >   describe "amqpSpec" $ do
-- >     it "can write and read a message" $ \conn -> do
-- >       chan <- openChannel conn
-- >
-- >       -- declare a queue, exchange and binding
-- >       _ <- declareQueue chan newQueue {queueName = "myQueue"}
-- >       declareExchange chan newExchange {exchangeName = "myExchange", exchangeType = "direct"}
-- >       bindQueue chan "myQueue" "myExchange" "myKey"
-- >
-- >       -- publish a message to our new exchange
-- >       let body = "hello world"
-- >       _ <-
-- >         publishMsg
-- >           chan
-- >           "myExchange"
-- >           "myKey"
-- >           newMsg
-- >             { msgBody = body,
-- >               msgDeliveryMode = Just Persistent
-- >             }
-- >
-- >       mMesg <- getMsg chan Ack "myQueue"
-- >       case mMesg of
-- >         Nothing -> expectationFailure "Should have received a message"
-- >         Just (m, e) -> do
-- >           msgBody m `shouldBe` body
-- >           ackEnv e
amqpSpec :: TestDefM (RabbitMQHandle ': outers) AMQP.Connection result -> TestDefM outers inner result
amqpSpec = rabbitMQSpec . setupAroundWith' (\serverHandle _ -> amqpConnectionSetupFunc serverHandle)

-- | Setup function for a connection to a given rabbitmq server
amqpConnectionSetupFunc :: RabbitMQHandle -> SetupFunc Connection
amqpConnectionSetupFunc h = do
  let opts = defaultConnectionOpts {coServers = [("localhost", rabbitMQHandlePort h)]}
  let acquire = openConnection'' opts
  let release = closeConnection
  bracketSetupFunc acquire release
