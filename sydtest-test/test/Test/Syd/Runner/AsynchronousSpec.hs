{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Runner.AsynchronousSpec (spec) where

import Control.Exception
import GHC.Stack (emptyCallStack)
import System.Timeout (timeout)
import Test.Syd
import Test.Syd.OptParse

-- | A forest with one test whose test function throws instead of producing a
-- result.
--
-- No test written with 'it' does this: whatever the test throws is turned into
-- a result.  This forest is built by hand because the point is what the runner
-- does when a test function throws anyway, for whatever reason: the thread
-- that collects results is then waiting for a result that will never come.
throwingTestForest :: TestForest '[] ()
throwingTestForest =
  [ DefSpecifyNode
      "throws instead of producing a result"
      TDef
        { testDefVal = \_progressReporter _wrapper -> throwIO (userError "this test function throws"),
          testDefCallStack = emptyCallStack
        }
      ()
  ]

-- | Each of these tears a whole nested runner down mid-flight, which cancels
-- its worker, printer and scheduler threads.  Running that alongside the rest
-- of the suite destabilised the suite under load, so keep it to itself.
spec :: Spec
spec = sequential $ describe "the asynchronous runner" $ do
  it "propagates an exception that escapes a test function out of runSpecForestAsynchronously" $ do
    let settings = defaultSettings {settingThreads = Asynchronous 2}
    -- The timeout is here so that a runner waiting forever for a result that
    -- will never come shows up as a failure instead of a stuck test suite.
    result <- timeout 10_000_000 $ try $ runSpecForestAsynchronously settings 2 throwingTestForest
    case result of
      Nothing -> expectationFailure "the asynchronous runner hung instead of propagating the exception"
      Just (Right _) -> expectationFailure "the asynchronous runner produced a result forest instead of propagating the exception"
      Just (Left (e :: SomeException)) -> fromException e `shouldBe` Just (userError "this test function throws")

  it "propagates an exception that escapes a test function out of runSpecForestInterleavedWithOutputAsynchronously" $ do
    let settings =
          defaultSettings
            { settingThreads = Asynchronous 2,
              settingOutputFormat = OutputFormatTerse
            }
    result <- timeout 10_000_000 $ try $ runSpecForestInterleavedWithOutputAsynchronously settings 2 throwingTestForest
    case result of
      Nothing -> expectationFailure "the asynchronous runner hung instead of propagating the exception"
      Just (Right _) -> expectationFailure "the asynchronous runner produced a result forest instead of propagating the exception"
      Just (Left (e :: SomeException)) -> fromException e `shouldBe` Just (userError "this test function throws")
