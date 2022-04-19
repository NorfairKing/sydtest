module Test.Syd.Webdriver.ScreenshotSpec (spec) where

import Network.URI
import Test.Syd
import Test.Syd.Wai
import Test.Syd.Webdriver
import Test.Syd.Webdriver.Screenshot
import Test.Syd.Webdriver.Screenshot.App

spec :: Spec
spec = exampleAppSpec $ do
  it "can make a screenshot of home" $ do
    openPath "/"
    goldenScreenshotHere "test_resources/home.png"

exampleAppSpec :: WebdriverSpec () -> Spec
exampleAppSpec = webdriverSpec $ \_ -> do
  portNumber <- applicationSetupFunc exampleApplication
  let uriStr = "http://127.0.0.1:" <> show portNumber
  case parseURI uriStr of
    Nothing -> liftIO $ expectationFailure $ "Failed to parse uri as string: " <> show uriStr
    Just uri -> pure (uri, ())
