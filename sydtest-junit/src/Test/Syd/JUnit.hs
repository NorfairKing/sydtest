{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.JUnit (renderJUnitSuites) where

import Control.Exception
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Test.Syd
import Test.Syd.SpecDef
import Text.XML.JUnit

renderJUnitSuites :: Timed ResultForest -> [TestSuite]
renderJUnitSuites = goForest [] . timedValue
  where
    goForest :: [Text] -> ResultForest -> [TestSuite]
    goForest path = concatMap (goTree path)

    goTree :: [Text] -> ResultTree -> [TestSuite]
    goTree path = \case
      SpecifyNode description def ->
        let Timed {..} = testDefVal def
            TestRunReport {..} = timedValue
            TestRunResult {..} = NE.last testRunReportRawResults
            success = case (testRunResultStatus, testRunReportExpectationMode) of
              (TestPassed, ExpectPassing) -> True
              (TestFailed, ExpectFailing) -> True
              _ -> False
            timeF = time (fromIntegral (timedEnd - timedBegin) / 1_000_000_000)
            messageF = case testRunResultException of
              Nothing -> id
              Just e -> failureMessage (T.pack (displayException e))
            suiteName = T.intercalate "." path
            suite =
              if success
                then inSuite suiteName $ timeF $ passed description
                else inSuite suiteName $ messageF $ timeF $ failed description
         in [suite]
      PendingNode description _ ->
        let suiteName = T.intercalate "." path
            suite = inSuite suiteName $ skipped description
         in [suite]
      DescribeNode p def -> goForest (path ++ [p]) def
      SubForestNode f -> goForest path f
