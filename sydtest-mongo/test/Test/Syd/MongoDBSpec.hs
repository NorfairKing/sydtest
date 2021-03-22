{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.MongoDBSpec (spec) where

import Data.Text (Text)
import Database.MongoDB as Mongo
import Test.Syd
import Test.Syd.MongoDB

spec :: Spec
spec = do
  describe "mongoServerSpec" $
    mongoServerSpec $
      it "sets up and tears down the mongo server nicely" $ do
        pure () :: IO ()
  describe "mongoSpec" $
    mongoSpec $ do
      it "can write and read an example value" $ \pipe -> do
        Mongo.access pipe master "example-database" $ do
          let collection = "example-collection"
              exampleVal = ["hello" =: ("world" :: Text)]
          i <- insert collection exampleVal
          r <- findOne (select ["_id" =: i] collection)
          liftIO $ r `shouldBe` Just (("_id" =: i) : exampleVal)
        pure () :: IO ()
      doNotRandomiseExecutionOrder $
        describe "shared state" $ do
          it "can write an example value" $ \pipe -> do
            Mongo.access pipe master "example-database" $ do
              let collection = "example-collection"
                  exampleVal = ["hello" =: ("world" :: Text)]
              insert_ collection exampleVal
            pure () :: IO ()
          it "cannot read a value that has not been written yet" $ \pipe -> do
            Mongo.access pipe master "example-database" $ do
              let collection = "example-collection"
              r <- find (select [] collection) >>= rest
              liftIO $ r `shouldBe` []
            pure () :: IO ()
