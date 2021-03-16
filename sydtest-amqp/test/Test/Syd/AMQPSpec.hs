module Test.Syd.AMQPSpec (spec) where

import Test.Syd
import Test.Syd.AMQP

spec :: Spec
spec = do
  describe "runRabbitMQServer" $
    it "can run the server" $ do
      runRabbitMQServer
