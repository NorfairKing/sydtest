module Test.Syd.ConcurrencyReproSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import System.Timeout (timeout)
import Test.Syd
import Test.Syd.OptParse (Settings (..), Threads (..), defaultSettings)
import Test.Syd.Runner (sydTestResult)

spec :: Spec
spec = describe "Fail-fast regressions" failFastSpec

failFastSpec :: Spec
failFastSpec =
    it "does not deadlock when multiple tests fail while fail-fast is enabled" $ do
      let settings :: Settings
          settings =
            defaultSettings
              { settingThreads = Asynchronous 4,
                settingFailFast = True,
                settingRetries = 0
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
      let iterations = 10000
          perRunTimeoutMicroSeconds = 2 * 1000 * 1000
      forM_ [1 .. iterations] $ \iteration -> do
        mTimed <- timeout perRunTimeoutMicroSeconds (sydTestResult settings failingSpec)
        case mTimed of
          Nothing ->
            expectationFailure ("Async runner deadlocked under fail-fast (iteration " ++ show iteration ++ ")")
          Just timed ->
            when (not (shouldExitFail settings (timedValue timed))) $ do
              let forest = timedValue timed
                  renderPath = intercalate " / " . map T.unpack
                  renderReport (path, tdef) =
                    let timedReport = testDefVal tdef
                        report = timedValue timedReport
                        status = testRunReportStatus settings report
                        rawStatuses = NE.toList $ NE.map testRunResultStatus (testRunReportRawResults report)
                     in renderPath path
                          ++ ": status="
                          ++ show status
                          ++ ", raw="
                          ++ show rawStatuses
                  rendered = show (map renderReport (flattenSpecForest forest))
              expectationFailure
                ( "Async runner unexpectedly succeeded (iteration "
                    ++ show iteration
                    ++ ") with results:\n"
                    ++ rendered
                )
