{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-warn-missing-methods -fno-warn-partial-fields -fno-warn-incomplete-uni-patterns -fno-warn-incomplete-record-updates #-}

module Main where

import Control.Exception
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TLB
import Spec (spec)
import Test.Syd
import Test.Syd.OptParse

main :: IO ()
main = do
  settings <- getSettings
  testForest <- execTestDefM settings spec

  putStrLn "Synchronous, non-interleaved"
  rf1 <- runSpecForestSynchronously settings testForest
  printOutputSpecForest settings rf1

  putStrLn "Synchronous, interleaved"
  _ <- runSpecForestInterleavedWithOutputSynchronously settings testForest

  putStrLn "Asynchronous, non-interleaved"
  rf2 <- runSpecForestAsynchronously settings 8 testForest
  printOutputSpecForest settings rf2

  putStrLn "Asynchronous, interleaved"
  _ <- runSpecForestInterleavedWithOutputAsynchronously settings 8 testForest

  putStrLn "Golden test of output"
  sydTest $
    describe "Golden Output" $ do
      it "renders output in the same way as before" $
        goldenByteStringFile "test_resources/output-test.txt" $ do
          testForestInOrder <- execTestDefM defaultSettings $ doNotRandomiseExecutionOrder spec
          rf <- runSpecForestSynchronously defaultSettings testForestInOrder
          pure
            $ TE.encodeUtf8
              . LT.toStrict
              . TLB.toLazyText
              . renderPrettyReport defaultSettings
            $ eraseTiming rf
      it "renders terse output in the same way as before" $
        goldenByteStringFile "test_resources/output-test-terse.txt" $ do
          testForestInOrder <- execTestDefM defaultSettings $ doNotRandomiseExecutionOrder spec
          rf <- runSpecForestSynchronously defaultSettings testForestInOrder
          pure
            $ TE.encodeUtf8
              . LT.toStrict
              . TLB.toLazyText
              . renderTerseSummary defaultSettings
            $ eraseTiming rf
  where
    tidySomeException :: SomeException -> SomeException
    tidySomeException se =
      let ?exceptionContext = mempty
       in case fromException se of
            Just (Contextual innerE ctx) ->
              -- Recursively clean the inner exception, wrap cleaned SomeException in Contextual
              let cleanedInner = tidySomeException (SomeException innerE)
               in toException $ Contextual cleanedInner ctx
            Nothing -> case fromException se of
              Just (ErrorCallWithLocation err _) ->
                toException $ ErrorCallWithLocation err ""
              Nothing ->
                -- Re-wrap to strip ExceptionContext
                case se of
                  SomeException e -> toException e

    -- Clears the exception context (backtrace) because that contains an
    -- unstable hash when rendered.
    -- We would prefer to just strip out the unstable hash but GHC doesn't
    -- seem to support that at the moment.
    tidyTestRunResult :: TestRunResult -> TestRunResult
    tidyTestRunResult trr = trr {testRunResultException = fmap tidySomeException (testRunResultException trr)}
    tidyTestRunReport :: TestRunReport -> TestRunReport
    tidyTestRunReport trr =
      trr
        { testRunReportRawResults = NE.map tidyTestRunResult (testRunReportRawResults trr)
        }
    eraseTimed :: Timed a -> Timed a
    eraseTimed t =
      t
        { timedBegin = 0,
          timedEnd = 0,
          timedWorker = 0
        }
    tidyTDef :: TDef (Timed TestRunReport) -> TDef (Timed TestRunReport)
    tidyTDef = fmap (eraseTimed . fmap tidyTestRunReport)
    erasedTimedInResultForest :: ResultForest -> ResultForest
    erasedTimedInResultForest = fmap (fmap tidyTDef)
    eraseTiming :: Timed ResultForest -> Timed ResultForest
    eraseTiming = fmap erasedTimedInResultForest . eraseTimed
