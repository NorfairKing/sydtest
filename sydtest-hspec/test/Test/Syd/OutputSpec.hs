{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.OutputSpec (spec) where

import Control.Exception (AssertionFailed (..), throwIO)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TLB
import Test.Hspec
import qualified Test.Syd as Syd
import Test.Syd.Hspec (fromHspec)
import Test.Syd.OptParse (getSettings)

spec :: Syd.Spec
spec =
  Syd.describe "Golden Output" $
    Syd.it "renders output in the same way as before" $
      Syd.goldenTextFile "test_resources/output-test.txt" $ do
        settings <- getSettings
        testForest <- Syd.execTestDefM settings (fromHspec hspecSpec)
        res <- Syd.runSpecForestSynchronously settings testForest

        pure
          $ LT.toStrict
            . TLB.toLazyText
            . Syd.renderPrettyReport settings
          $ eraseTiming res

hspecSpec :: Spec
hspecSpec = do
  it "failure with no reason" False

  it "failure with reason" $ expectationFailure "failure reason"

  it "failure with \"expected x but got y\"" $ 8 `shouldBe` (6 :: Int)

  it "failure with error" $ do
    throwIO (AssertionFailed "error msg") :: IO ()

eraseTiming :: Syd.Timed Syd.ResultForest -> Syd.Timed Syd.ResultForest
eraseTiming = fmap erasedTimedInResultForest . eraseTimed
  where
    eraseTimed :: Syd.Timed a -> Syd.Timed a
    eraseTimed t =
      t
        { Syd.timedBegin = 0,
          Syd.timedEnd = 0,
          Syd.timedWorker = 0
        }

    erasedTimedInResultForest :: Syd.ResultForest -> Syd.ResultForest
    erasedTimedInResultForest = fmap (fmap (fmap eraseTimed))
