{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.AMQP where

import Control.Exception
import Network.AMQP as AMQP
import Test.Syd
import Test.Syd.RabbitMQ

amqpSpec :: TestDefM (RabbitMQHandle ': outers) AMQP.Connection result -> TestDefM outers () result
amqpSpec = rabbitMQSpec . setupAroundWith' amqpConnectionSetupFunc

amqpConnectionSetupFunc :: RabbitMQHandle -> SetupFunc () Connection
amqpConnectionSetupFunc h =
  makeSimpleSetupFunc $ \func -> do
    let opts = defaultConnectionOpts {coServers = [("localhost", rabbitMQHandlePort h)]}
    let acquire = openConnection'' opts
    let release = closeConnection
    let use = func
    bracket acquire release use
