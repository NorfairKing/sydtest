module Test.Syd.RedisSpec (spec) where

import Test.Syd
import Test.Syd.Redis

spec :: Spec
spec =
  describe "redisServerSpec" $
    redisServerSpec $
      it "sets up and tears down the redis server nicely" $ do
        pure () :: IO ()
