{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd where

import Control.Exception
import System.Exit

type Test = IO ()

data TestRunResult = TestPassed | TestFailed
  deriving (Show)

runTest :: Test -> IO TestRunResult
runTest func =
  (func >>= (evaluate . (`seq` TestPassed)))
    `catches` [ Handler $ \(_ :: ErrorCall) -> pure TestFailed,
                Handler $ \(_ :: ExitCode) -> pure TestFailed,
                Handler $ \(_ :: RecConError) -> pure TestFailed,
                Handler $ \(_ :: RecSelError) -> pure TestFailed,
                Handler $ \(_ :: RecUpdError) -> pure TestFailed,
                Handler $ \(_ :: PatternMatchFail) -> pure TestFailed
              ]
