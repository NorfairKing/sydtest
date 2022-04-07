{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.OptParseSpec (spec) where

import Test.Syd
import Test.Syd.OptParse

spec :: Spec
spec = do
  describe "combineToSettings" $ do
    it "works for this default settinsg example" $
      do
        let flags = defaultFlags
            environment = defaultEnvironment
            mConf = Nothing
            settings = defaultSettings
        combineToSettings flags environment mConf `shouldReturn` settings

    it "works for this debug example" $ do
      let flags = defaultFlags {flagDebug = Just True}
          environment = defaultEnvironment
          mConf = Nothing
          settings =
            defaultSettings
              { settingThreads = Synchronous,
                settingRandomiseExecutionOrder = False,
                settingFailFast = True,
                settingReportProgress = ReportProgress,
                settingDebug = True
              }
      combineToSettings flags environment mConf `shouldReturn` settings
