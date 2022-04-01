module Test.Syd.ServantWithContextSpec (spec) where

import Control.Concurrent.STM
import Servant
import Servant.Client
import Test.Syd
import Test.Syd.Servant
import Test.Syd.Servant.ExampleWithContext
import Test.Syd.Wai

exampleSetupFunc :: SetupFunc (Server ExampleAPI)
exampleSetupFunc = SetupFunc $ \func -> do
  var <- newTVarIO 0
  func $ exampleServer var

spec :: Spec
spec = servantSpecWithSetupFuncWithContext exampleAPI exampleContext exampleSetupFunc $ do
  describe "requests with correct credentials" $ do
    it "gets zero at the start" $ do
      r <- clientGetCorrectCredentials
      liftIO $ r `shouldBe` 0
    it "can add 1 to get 1" $ do
      NoContent <- clientAddCorrectCredentials 1
      r <- clientGetCorrectCredentials
      liftIO $ r `shouldBe` 1
  describe "requests with incorrect credentials" $ do
    it "doesn't allow GET requests without correct credentials" $ \clientEnv -> do
      errOrResult <- testClientOrError clientEnv clientGetWrongCredentials
      case errOrResult of
        Left (FailureResponse _ res) -> responseStatusCode res `shouldBe` forbidden403
        res -> expectationFailure $ "Expected a failed request with status 403, but got " <> show res
    it "doesn't allow POST requests without correct credentials" $ \clientEnv -> do
      errOrResult <- testClientOrError clientEnv $ clientAddWrongCredentials 1
      case errOrResult of
        Left (FailureResponse _ res) -> responseStatusCode res `shouldBe` forbidden403
        res -> expectationFailure $ "Expected a failed request with status 403, but got " <> show res
