{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.MongoDBSpec (spec) where

import Test.Syd
import Test.Syd.MongoDB

spec :: Spec
spec = do
  describe "mongoServerSpec" $
    mongoServerSpec $
      it "sets up and tears down the mongo server nicely" $ do
        pure () :: IO ()
