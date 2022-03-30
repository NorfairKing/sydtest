module Test.Syd.ServantWithContextSpec (spec) where

import Control.Concurrent.STM
import Servant
import Test.Syd
import Test.Syd.Servant
import Test.Syd.Servant.ExampleWithContext

exampleSetupFunc :: SetupFunc (Server ExampleAPI)
exampleSetupFunc = SetupFunc $ \func -> do
  var <- newTVarIO 0
  func $ exampleServer var

spec :: Spec
spec = servantSpecWithSetupFuncWithContext exampleAPI exampleContext exampleSetupFunc $ do
  it "gets zero at the start" $ do
    r <- clientGetCorrectCredentials
    liftIO $ r `shouldBe` 0
  it "can add 1 to get 1" $ do
    NoContent <- clientAddCorrectCredentials 1
    r <- clientGetCorrectCredentials
    liftIO $ r `shouldBe` 1
