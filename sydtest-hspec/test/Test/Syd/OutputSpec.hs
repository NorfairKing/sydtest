{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.OutputSpec (spec) where

import Control.Exception (AssertionFailed (..), throwIO)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TLB
import Test.Hspec
import qualified Test.Syd as Syd
import Test.Syd.Hspec (fromHspec)
import Test.Syd.OptParse (getSettings)
import Test.Syd.TestUtils
import Text.Colour

spec :: Syd.Spec
spec =
  Syd.describe "Golden Output" $
    Syd.it "renders output in the same way as before" $
      Syd.goldenTextFile "test_resources/output-test.txt" $ do
        settings <- getSettings
        testForest <- Syd.execTestDefM settings (fromHspec hspecSpec)
        res <- Syd.timeItT 0 $ Syd.runSpecForestSynchronously settings testForest

        pure
          $ LT.toStrict
            . TLB.toLazyText
            . Syd.renderResultReport settings WithoutColours
          $ eraseTiming res

hspecSpec :: Spec
hspecSpec = do
  it "failure with no reason" False

  it "failure with reason" $ expectationFailure "failure reason"

  it "failure with \"expected x but got y\"" $ 8 `shouldBe` (6 :: Int)

  it "failure with error" $ do
    throwIO (AssertionFailed "error msg") :: IO ()
