module Test.Syd.ServantSpec (spec) where

import Control.Concurrent.STM
import Servant
import Test.Syd
import Test.Syd.Servant
import Test.Syd.Servant.Example

exampleSetupFunc :: SetupFunc () (Server ExampleAPI)
exampleSetupFunc = SetupFunc $ \func () -> do
  var <- newTVarIO 0
  func $ exampleServer var

spec :: Spec
spec = servantSpecWithSetupFunc exampleAPI exampleSetupFunc $ do
  it "gets zero at the start" $ \cenv -> do
    r <- testClient cenv clientGet
    r `shouldBe` 0
  it "can add 1 to get 1" $ \cenv -> do
    r <- testClient cenv $ do
      NoContent <- clientAdd 1
      clientGet
    r `shouldBe` 1
