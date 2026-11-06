{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Runner.AsynchronousSpec (spec) where

import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import GHC.Stack (emptyCallStack)
import System.Timeout (timeout)
import Test.Syd
import Test.Syd.OptParse

-- | Some of these tear a whole nested runner down mid-flight, which cancels
-- its worker, printer and scheduler threads.
-- Running that alongside the rest of the suite destabilised the suite under
-- load, so keep it to itself.
spec :: Spec
spec = sequential $ describe "the asynchronous runner" $ do
  -- The 'IsTest' instances in sydtest turn whatever a test throws into a result,
  -- except the exceptions that stop the suite.
  -- These forests are built by hand, to see what the runner does with an
  -- instance that does not.
  it "fails a test whose test function throws, in runSpecForestAsynchronously" $ do
    let settings =
          defaultSettings
            { settingThreads = Asynchronous 2,
              settingRetries = 1
            }
    let forest :: TestForest '[] ()
        forest =
          [ DefSpecifyNode
              "throws instead of producing a result"
              TDef
                { testDefVal = \_progressReporter _wrapper -> throwIO (userError "this test function throws"),
                  testDefCallStack = emptyCallStack
                }
              ()
          ]
    -- The timeout is here so that a runner waiting forever for a result that
    -- will never come shows up as a failure instead of a stuck test suite.
    mResultForest <- timeout 10_000_000 $ runSpecForestAsynchronously settings 2 forest
    case mResultForest of
      Nothing -> expectationFailure "the asynchronous runner hung"
      Just resultForest ->
        map
          (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
          (flattenSpecForest (timedValue resultForest))
          `shouldBe` [(TestFailed, Just (userError "this test function throws")) :| []]

  it "fails a test whose test function throws, in runSpecForestInterleavedWithOutputAsynchronously" $ do
    let settings =
          defaultSettings
            { settingThreads = Asynchronous 2,
              settingOutputFormat = OutputFormatTerse,
              settingRetries = 1
            }
    let forest :: TestForest '[] ()
        forest =
          [ DefSpecifyNode
              "throws instead of producing a result"
              TDef
                { testDefVal = \_progressReporter _wrapper -> throwIO (userError "this test function throws"),
                  testDefCallStack = emptyCallStack
                }
              ()
          ]
    mResultForest <- timeout 10_000_000 $ runSpecForestInterleavedWithOutputAsynchronously settings 2 forest
    case mResultForest of
      Nothing -> expectationFailure "the asynchronous runner hung"
      Just resultForest ->
        map
          (NE.map (\r -> (testRunResultStatus r, testRunResultException r >>= fromException)) . testRunReportRawResults . timedValue . testDefVal . snd)
          (flattenSpecForest (timedValue resultForest))
          `shouldBe` [(TestFailed, Just (userError "this test function throws")) :| []]

  -- An exception that stops the suite escapes the test function, so the
  -- result-collecting thread must not keep waiting for the result.
  it "propagates an exception that stops the suite out of runSpecForestAsynchronously" $ do
    let settings = defaultSettings {settingThreads = Asynchronous 2}
    let forest :: TestForest '[] ()
        forest =
          [ DefSpecifyNode
              "throws an exception that stops the suite"
              TDef
                { testDefVal = \_progressReporter _wrapper -> throwIO ThreadKilled,
                  testDefCallStack = emptyCallStack
                }
              ()
          ]
    result <- timeout 10_000_000 $ try $ runSpecForestAsynchronously settings 2 forest
    case result of
      Nothing -> expectationFailure "the asynchronous runner hung instead of propagating the exception"
      Just (Right _) -> expectationFailure "the asynchronous runner produced a result forest instead of propagating the exception"
      Just (Left (e :: SomeException)) -> fromException e `shouldBe` Just ThreadKilled

  it "propagates an exception that stops the suite out of runSpecForestInterleavedWithOutputAsynchronously" $ do
    let settings =
          defaultSettings
            { settingThreads = Asynchronous 2,
              settingOutputFormat = OutputFormatTerse
            }
    let forest :: TestForest '[] ()
        forest =
          [ DefSpecifyNode
              "throws an exception that stops the suite"
              TDef
                { testDefVal = \_progressReporter _wrapper -> throwIO ThreadKilled,
                  testDefCallStack = emptyCallStack
                }
              ()
          ]
    result <- timeout 10_000_000 $ try $ runSpecForestInterleavedWithOutputAsynchronously settings 2 forest
    case result of
      Nothing -> expectationFailure "the asynchronous runner hung instead of propagating the exception"
      Just (Right _) -> expectationFailure "the asynchronous runner produced a result forest instead of propagating the exception"
      Just (Left (e :: SomeException)) -> fromException e `shouldBe` Just ThreadKilled
