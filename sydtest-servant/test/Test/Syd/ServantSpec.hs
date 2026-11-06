module Test.Syd.ServantSpec (spec) where

import Control.Concurrent.STM
import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Servant
import Servant.Client (ClientM)
import Test.Syd
import Test.Syd.OptParse
import Test.Syd.Servant
import Test.Syd.Servant.Example

exampleSetupFunc :: SetupFunc (Server ExampleAPI)
exampleSetupFunc = SetupFunc $ \func -> do
  var <- newTVarIO 0
  func $ exampleServer var

-- | An exception that a test throws itself, but that is classified as
-- asynchronous, as @effectful@'s @Error@ effect does.
-- See https://github.com/NorfairKing/sydtest/issues/110
data SynchronousButClassifiedAsynchronous = SynchronousButClassifiedAsynchronous
  deriving (Show, Eq)

instance Exception SynchronousButClassifiedAsynchronous where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

spec :: Spec
spec = do
  describe "a test that throws an asynchronously-classified exception fails with that exception" $ do
    let settings = defaultSettings {settingRetries = 1}

    it "ClientM ()" $ do
      specForest <-
        execTestDefM settings $
          servantSpecWithSetupFunc exampleAPI exampleSetupFunc $
            it "throws" (liftIO (throwIO SynchronousButClassifiedAsynchronous) :: ClientM ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "outerArgs -> ClientM ()" $ do
      specForest <-
        execTestDefM settings $
          servantSpecWithSetupFunc exampleAPI exampleSetupFunc $
            itWithBoth "throws" (\_ -> liftIO (throwIO SynchronousButClassifiedAsynchronous) :: ClientM ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

  servantSpecWithSetupFunc exampleAPI exampleSetupFunc $ do
    it "gets zero at the start" $ do
      r <- clientGet
      liftIO $ r `shouldBe` 0
    it "can add 1 to get 1" $ do
      NoContent <- clientAdd 1
      r <- clientGet
      liftIO $ r `shouldBe` 1
