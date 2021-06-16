module Test.Syd.ServantSpec (spec) where

import Control.Concurrent.STM
import Servant
import Test.Syd
import Test.Syd.Servant
import Test.Syd.Servant.Example

exampleSetupFunc :: SetupFunc (Server ExampleAPI)
exampleSetupFunc = SetupFunc $ \func -> do
  var <- newTVarIO 0
  func $ exampleServer var

spec :: Spec
spec = servantSpecWithSetupFunc exampleAPI exampleSetupFunc $ do
  it "gets zero at the start" $ do
    r <- clientGet
    liftIO $ r `shouldBe` 0
  it "can add 1 to get 1" $ do
    NoContent <- clientAdd 1
    r <- clientGet
    liftIO $ r `shouldBe` 1
