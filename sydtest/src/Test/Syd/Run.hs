{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Run where

import Control.Exception
import Data.Aeson as JSON hiding (Result, Success)
import Data.Typeable
import GHC.Generics (Generic)
import System.Exit
import System.TimeIt
import Test.QuickCheck
import Test.QuickCheck.IO ()
import Test.Syd.Silence

class IsTest a where
  runTest :: a -> IO TestRunResult

instance IsTest Bool where
  runTest = runPureTest

runPureTest :: Bool -> IO TestRunResult
runPureTest b = do
  let testRunResultNumTests = Nothing
  (testRunResultExecutionTime, resultBool) <- timeItT $ (Right <$> evaluate b) `catches` pureExceptionHandlers
  let (testRunResultStatus, testRunResultException) = case resultBool of
        Left ex -> (TestFailed, Just ex)
        Right bool -> (if bool then TestPassed else TestFailed, Nothing)
  let testRunResultNumShrinks = Nothing
  pure TestRunResult {..}

instance IsTest (IO a) where
  runTest = runIOTest

runIOTest :: IO a -> IO TestRunResult
runIOTest func = do
  let testRunResultNumTests = Nothing
  (testRunResultExecutionTime, (testRunResultStatus, testRunResultException)) <-
    timeItT
      $ runInSilencedNiceProcess
      $ do
        result <-
          (func >>= (evaluate . (`seq` Right TestPassed)))
            `catches` ( [ Handler $ \(e :: ExitCode) -> pure (Left $ Left $ displayException e)
                        ]
                          ++ pureExceptionHandlers
                      )
        pure $ case result of
          Left ex -> (TestFailed, Just ex)
          Right r -> (r, Nothing)
  let testRunResultNumShrinks = Nothing
  pure TestRunResult {..}

instance IsTest Property where
  runTest = runPropertyTest

runPropertyTest :: Property -> IO TestRunResult
runPropertyTest p = do
  let args = stdArgs -- Customise args
  runInSilencedNiceProcess $ do
    (testRunResultExecutionTime, result) <- timeItT $ quickCheckWithResult args p
    testRunResultStatus <- pure $ case result of
      Success {} -> TestPassed
      GaveUp {} -> TestFailed
      Failure {} -> TestFailed
      NoExpectedFailure {} -> TestFailed
    let testRunResultNumTests = Just $ fromIntegral $ numTests result
    let testRunResultNumShrinks = case result of
          Failure {} -> Just $ fromIntegral $ numShrinks result
          _ -> Nothing
    let testRunResultException = case result of
          Failure {} -> do
            se <- theException result
            pure $ case fromException se of
              Just a -> Right a
              Nothing -> Left $ displayException se
          _ -> Nothing
    pure TestRunResult {..}

pureExceptionHandlers :: [Handler (Either (Either String Assertion) a)]
pureExceptionHandlers =
  [ Handler $ \(a :: Assertion) -> pure (Left $ Right a),
    Handler $ \(e :: ErrorCall) -> pure (Left (Left (displayException e))),
    Handler $ \(e :: RecConError) -> pure (Left (Left (displayException e))),
    Handler $ \(e :: RecSelError) -> pure (Left (Left (displayException e))),
    Handler $ \(e :: RecUpdError) -> pure (Left (Left (displayException e))),
    Handler $ \(e :: PatternMatchFail) -> pure (Left (Left (displayException e)))
  ]

type Test = IO ()

data TestRunResult
  = TestRunResult
      { testRunResultStatus :: !TestStatus,
        testRunResultException :: !(Maybe (Either String Assertion)),
        testRunResultNumTests :: !(Maybe Word),
        testRunResultNumShrinks :: !(Maybe Word),
        testRunResultExecutionTime :: !Double -- In seconds
      }
  deriving (Show, Generic)

instance FromJSON TestRunResult

instance ToJSON TestRunResult

data TestStatus = TestPassed | TestFailed
  deriving (Show, Eq, Generic)

instance FromJSON TestStatus

instance ToJSON TestStatus

data Assertion = Equality String String
  deriving (Show, Eq, Typeable, Generic)

instance Exception Assertion

instance FromJSON Assertion

instance ToJSON Assertion
