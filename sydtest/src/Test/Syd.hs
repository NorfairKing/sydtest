{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd
  ( module Test.Syd,
    module Test.Syd.Run,
    module Test.Syd.SpecForest,
    module Test.Syd.Expectation,
    module Test.Syd.Output,
    module Test.Syd.Silence,
    module Test.Syd.Def,
  )
where

import Control.Monad.Reader
import System.Exit
import Test.QuickCheck.IO ()
import Test.Syd.Def
import Test.Syd.Expectation
import Test.Syd.Output
import Test.Syd.Run
import Test.Syd.Silence
import Test.Syd.SpecForest

sydTest :: Spec -> IO ()
sydTest spec = do
  ((), specForest) <- runTestDefM spec
  resultForest <- runSpecForest specForest
  printOutputSpecForest resultForest
  when (shouldExitFail resultForest) (exitWith (ExitFailure 1))

runSpecForest :: TestForest -> IO ResultForest
runSpecForest = traverse $ traverse $ \td -> do
  let runFunc = testDefVal td
  result <- runFunc
  pure $ td {testDefVal = result}

shouldExitFail :: ResultForest -> Bool
shouldExitFail = any (any ((== TestFailed) . testRunResultStatus . testDefVal))
