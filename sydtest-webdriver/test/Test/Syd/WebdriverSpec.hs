module Test.Syd.WebdriverSpec (spec) where

import Path
import Path.IO
import Test.Syd
import Test.Syd.Path
import Test.Syd.Webdriver
import Test.Syd.Webdriver.App

spec :: Spec
spec = exampleAppSpec $ do
  it "can navigate to home" $
    openRoute HomeR

exampleAppSpec :: WebdriverSpec App -> Spec
exampleAppSpec = webdriverSpec $ \_ -> do
  tdir <- tempDirSetupFunc "sydtest-yesod"
  sessionKeyFile <- resolveFile tdir "client_session_key.aes"
  pure $ App {appSessionKeyFile = fromAbsFile sessionKeyFile}
