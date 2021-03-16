module Test.Syd.AMQPSpec (spec) where

import Test.Syd
import Test.Syd.AMQP

spec :: Spec
spec = rabbitMQSpec $ do
  describe "rabbitMQSpec" $
    it "can run the server and clean up nicely" $ do
      pure () :: IO ()
