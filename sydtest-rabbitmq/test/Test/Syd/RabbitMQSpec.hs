module Test.Syd.RabbitMQSpec (spec) where

import Test.Syd
import Test.Syd.RabbitMQ

spec :: Spec
spec = do
  rabbitMQSpec $ do
    describe "rabbitMQSpec" $
      it "can run the server and clean up nicely" $ do
        pure () :: IO ()
