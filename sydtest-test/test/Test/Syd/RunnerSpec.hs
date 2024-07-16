module Test.Syd.RunnerSpec (spec) where

import Control.Monad
import Test.Syd
import Test.Syd.OptParse

spec :: Spec
spec = do
  it "Can run an empty test suite with many runners." $ do
    let settings = defaultSettings {settingThreads = Asynchronous 1}
    let runs = 10000
    let simpleSpec :: Spec
        simpleSpec =
          it "is a simple assertion" $ do
            True `shouldBe` True
    specForest <- execTestDefM settings simpleSpec
    replicateM_ runs $ do
      void $ runSpecForestAsynchronously settings 1 specForest
