module Test.Syd.ConcurrencyReproSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import System.Timeout (timeout)
import Test.Syd
import Test.Syd.OptParse (Settings (..), Threads (..), defaultSettings)
import Test.Syd.Runner (sydTestResult)

spec :: Spec
spec = describe "Fail-fast regressions" failFastSpec

failFastSpec :: Spec
failFastSpec =
  parallel $
    it "does not deadlock when multiple tests fail while fail-fast is enabled" $ do
      let settings :: Settings
          settings =
            defaultSettings
              { settingThreads = Asynchronous 4,
                settingFailFast = True
              }
          failingSpec :: Spec
          failingSpec =
            parallel $ do
              it "fails-1" $ do
                threadDelay 1000
                (expectationFailure "boom" :: IO ())
              it "fails-2" $ do
                threadDelay 1000
                (expectationFailure "boom" :: IO ())
              it "fails-3" $ do
                threadDelay 1000
                (expectationFailure "boom" :: IO ())
              it "fails-4" $ do
                threadDelay 1000
                (expectationFailure "boom" :: IO ())
      mTimed <- timeout (2 * 1000 * 1000) (sydTestResult settings failingSpec)
      case mTimed of
        Nothing -> expectationFailure "Async runner deadlocked under fail-fast"
        Just timed -> when (not (shouldExitFail settings (timedValue timed))) $
          expectationFailure "Async runner unexpectedly succeeded"
