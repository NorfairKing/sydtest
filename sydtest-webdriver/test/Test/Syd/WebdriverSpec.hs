module Test.Syd.WebdriverSpec (spec) where

import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Network.URI
import Test.Syd
import Test.Syd.OptParse
import Test.Syd.Wai
import Test.Syd.Webdriver
import Test.Syd.Webdriver.App

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

    it "WebdriverTestM app ()" $ do
      specForest <-
        execTestDefM settings $
          exampleAppSpec $
            it "throws" (liftIO (throwIO SynchronousButClassifiedAsynchronous) :: WebdriverTestM () ())
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

    it "WebdriverTestM app (GoldenTest a)" $ do
      specForest <-
        execTestDefM settings $
          exampleAppSpec $
            it
              "throws"
              ( pure
                  ( GoldenTest
                      { goldenTestRead = pure (Just ()),
                        goldenTestProduce = throwIO SynchronousButClassifiedAsynchronous,
                        goldenTestWrite = \() -> pure (),
                        goldenTestCompare = \() () -> pure Nothing
                      }
                  ) ::
                  WebdriverTestM () (GoldenTest ())
              )
      resultForest <- runSpecForestSynchronously settings specForest
      map
        (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
        (flattenSpecForest (timedValue resultForest))
        `shouldBe` [(TestFailed, Just SynchronousButClassifiedAsynchronous) :| []]

  exampleAppSpec $ do
    it "can navigate to home" $
      openPath "/"

exampleAppSpec :: WebdriverSpec () -> Spec
exampleAppSpec = webdriverSpec $ \_ -> do
  portNumber <- applicationSetupFunc exampleApplication
  let uriStr = "http://127.0.0.1:" <> show portNumber
  case parseURI uriStr of
    Nothing -> liftIO $ expectationFailure $ "Failed to parse uri as string: " <> show uriStr
    Just uri -> pure (uri, ())
