{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.JUnit
  ( sydTestJUnit,
    sydTestJUnitWith,
    renderJUnitSuites,
  )
where

import Control.Exception
import Control.Monad
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Path.IO
import System.Exit
import Test.Syd
import Test.Syd.Def
import Test.Syd.Expectation
import Test.Syd.HList
import Test.Syd.Modify
import Test.Syd.OptParse
import Test.Syd.Output
import Test.Syd.ReRun
import Test.Syd.Run
import Test.Syd.Runner
import Test.Syd.SVG
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.XML.JUnit

sydTestJUnit :: Spec -> IO ()
sydTestJUnit spec = do
  sets <- getSettings
  sydTestJUnitWith sets spec

sydTestJUnitWith :: Settings -> Spec -> IO ()
sydTestJUnitWith sets spec = do
  resultForest <- withRerunByReport sets (sydTestResult sets) spec

  when (settingProfile sets) $ writeProfile resultForest

  let filename = "junit.xml"
  xmlFile <- resolveFile' filename
  writeXmlReport (fromAbsFile xmlFile) $ renderJUnitSuites resultForest
  putStrLn $ "Wrote JUnit report to: " ++ show xmlFile

  when (shouldExitFail sets (timedValue resultForest)) (exitWith (ExitFailure 1))

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
