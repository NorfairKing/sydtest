{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.GoldenSpec (spec) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import Test.Syd
import Test.Syd.OptParse
import Text.Colour

spec :: Spec
spec = do
  describe "outputResultForest" $ do
    it "outputs the same as last time" $ do
      pureGoldenTextFile
        "test_resources/output.golden"
        ( LT.toStrict $
            LTB.toLazyText $
              renderResultReport
                defaultSettings
                With24BitColours
                ( Timed
                    { timedValue = [],
                      timedWorker = 0,
                      timedBegin = 0,
                      timedEnd = 0
                    }
                )
        )
  describe "defaultSettings" $ do
    it "is the same thing as last time" $ goldenPrettyShowInstance "test_resources/defaultSettings-show.golden" defaultSettings
