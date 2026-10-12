{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Runner.AsynchronousSpec (spec) where

import Control.Exception
import System.Timeout (timeout)
import Test.Syd
import Test.Syd.OptParse

-- | An exception that is thrown synchronously by a test but classified as
-- asynchronous, the way @effectful@'s @Error@ effect and @async@'s
-- 'Control.Concurrent.Async.AsyncCancelled' are.
--
-- 'Test.Syd.Run.exceptionHandlers' deliberately re-throws these instead of
-- turning them into a test result, so they escape the test function.
data AsynchronouslyClassified = AsynchronouslyClassified
  deriving (Show, Eq)

instance Exception AsynchronouslyClassified where
  toException = asyncExceptionToException
  fromException = asyncExceptionFromException

spec :: Spec
spec = describe "the asynchronous runner" $ do
  it "propagates an asynchronously-classified exception out of runSpecForestAsynchronously" $ do
    let settings = defaultSettings {settingThreads = Asynchronous 2}
    specForest <-
      execTestDefM settings $
        it "throws an asynchronously-classified exception" (throwIO AsynchronouslyClassified :: IO ())
    result <- timeout 10_000_000 $ try $ runSpecForestAsynchronously settings 2 specForest
    case result of
      Nothing -> expectationFailure "the asynchronous runner hung instead of propagating the exception"
      Just (Right _) -> expectationFailure "the asynchronous runner produced a result forest instead of propagating the exception"
      Just (Left (e :: SomeException)) -> fromException e `shouldBe` Just AsynchronouslyClassified

  it "propagates an asynchronously-classified exception out of runSpecForestInterleavedWithOutputAsynchronously" $ do
    let settings =
          defaultSettings
            { settingThreads = Asynchronous 2,
              settingOutputFormat = OutputFormatTerse
            }
    specForest <-
      execTestDefM settings $
        it "throws an asynchronously-classified exception" (throwIO AsynchronouslyClassified :: IO ())
    result <- timeout 10_000_000 $ try $ runSpecForestInterleavedWithOutputAsynchronously settings 2 specForest
    case result of
      Nothing -> expectationFailure "the asynchronous runner hung instead of propagating the exception"
      Just (Right _) -> expectationFailure "the asynchronous runner produced a result forest instead of propagating the exception"
      Just (Left (e :: SomeException)) -> fromException e `shouldBe` Just AsynchronouslyClassified
