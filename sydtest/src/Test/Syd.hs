{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Syd where

import Control.Exception
import Control.Monad.Reader
import Data.Aeson as JSON ((.:), (.=), FromJSON (..), ToJSON (..), eitherDecode, encode, object, withObject)
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.IORef
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)
import Rainbow
import System.Exit
import System.IO (hClose, hFlush)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.IO ()
import Test.QuickCheck.Test

-- For the Testable (IO ()) instance

sydTest :: Spec -> IO ()
sydTest spec = do
  ((), specForest) <- runTestDefM spec
  resultForest <- runSpecForest specForest
  printOutputSpecForest resultForest
  when (shouldExitFail resultForest) (exitWith (ExitFailure 1))

type Spec = TestDefM ()

data TestDefEnv
  = TestDefEnv
      { testDefEnvForest :: IORef TestForest
      }

newtype TestDefM a = TestDefM {unTestDefM :: ReaderT TestDefEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestDefEnv)

runTestDefM :: TestDefM a -> IO (a, TestForest)
runTestDefM defFunc = do
  forestVar <- newIORef []
  let env = TestDefEnv {testDefEnvForest = forestVar}
  let func = unTestDefM defFunc
  a <- runReaderT func env
  sf <- readIORef forestVar
  pure (a, sf)

describe :: String -> TestDefM a -> TestDefM a
describe s func = do
  (a, sf) <- liftIO $ runTestDefM func
  var <- asks testDefEnvForest
  liftIO $ modifyIORef var $ (++ [DescribeNode s sf]) -- FIXME this can probably be slow
  pure a

it :: IsTest test => String -> test -> TestDefM ()
it s t = do
  var <- asks testDefEnvForest
  liftIO $ modifyIORef var $ (++ [SpecifyNode s $ runTest t]) -- FIXME this can probably be slow

type SpecForest a = [SpecTree a]

data SpecTree a
  = DescribeNode String (SpecForest a) -- A description
  | SpecifyNode String a -- A test with its description
  deriving (Show, Functor)

instance Foldable SpecTree where
  foldMap f = \case
    DescribeNode _ sts -> foldMap (foldMap f) sts
    SpecifyNode _ a -> f a

instance Traversable SpecTree where
  traverse func = \case
    DescribeNode s sts -> DescribeNode s <$> traverse (traverse func) sts
    SpecifyNode s a -> SpecifyNode s <$> func a

type TestForest = SpecForest (IO TestRunResult)

type ResultForest = SpecForest TestRunResult

runSpecForest :: TestForest -> IO ResultForest
runSpecForest = traverse (traverse id)

printOutputSpecForest :: ResultForest -> IO ()
printOutputSpecForest results = do
  byteStringMaker <- byteStringMakerFromEnvironment
  let bytestrings = map (chunksToByteStrings byteStringMaker) (outputSpecForest results) :: [[ByteString]]
  forM_ bytestrings $ \bs -> do
    mapM_ SB.putStr bs
    SB8.putStrLn ""

outputSpecForest :: ResultForest -> [[Chunk]]
outputSpecForest = concatMap outputSpecTree

outputSpecTree :: SpecTree TestRunResult -> [[Chunk]]
outputSpecTree = \case
  DescribeNode s sf -> [fore yellow $ chunk (T.pack s)] : map (chunk "  " :) (outputSpecForest sf)
  SpecifyNode s TestRunResult {..} ->
    [ map
        (fore (statusColour testRunResultStatus))
        [ chunk (statusCheckMark testRunResultStatus),
          chunk (T.pack s)
        ]
    ]

statusColour :: TestStatus -> Radiant
statusColour = \case
  TestPassed -> green
  TestFailed -> red

statusCheckMark :: TestStatus -> Text
statusCheckMark = \case
  TestPassed -> "\10003 "
  TestFailed -> "\10007 "

shouldExitFail :: SpecForest TestRunResult -> Bool
shouldExitFail = any (any ((== TestFailed) . testRunResultStatus))

class IsTest a where
  runTest :: a -> IO TestRunResult

instance IsTest Bool where
  runTest = runPureTest

runPureTest :: Bool -> IO TestRunResult
runPureTest b = do
  resultBool <-
    (evaluate b)
      `catches` (pureExceptionHandlers False)
  let testRunResultStatus = if resultBool then TestPassed else TestFailed
  pure TestRunResult {..}

instance IsTest (IO a) where
  runTest = runIOTest

runIOTest :: IO a -> IO TestRunResult
runIOTest func = do
  testRunResultStatus <-
    runInSilencedNiceProcess $
      (func >>= (evaluate . (`seq` TestPassed)))
        `catches` ( [ Handler $ \(_ :: ExitCode) -> pure TestFailed
                    ]
                      ++ (pureExceptionHandlers TestFailed)
                  )
  pure TestRunResult {..}

instance IsTest Property where
  runTest = runPropertyTest

deriving instance Generic Result

instance FromJSON Result where
  parseJSON = withObject "Result" $ \o -> do
    t <- o .: "type"
    case (t :: Text) of
      "success" ->
        Success
          <$> o .: "num-tests"
          <*> o .: "num-discarded"
          <*> o .: "labels"
          <*> o .: "classes"
          <*> o .: "tables"
          <*> o .: "output"
      "gave-up" ->
        GaveUp
          <$> o .: "num-tests"
          <*> o .: "num-discarded"
          <*> o .: "labels"
          <*> o .: "classes"
          <*> o .: "tables"
          <*> o .: "output"
      "failure" ->
        Failure
          <$> o .: "num-tests"
          <*> o .: "num-discarded"
          <*> o .: "num-shrinks"
          <*> o .: "num-shrink-tries"
          <*> o .: "num-shrink-final"
          <*> (pure (error "dummy seed"))
          <*> o .: "size"
          <*> o .: "reason"
          <*> (pure (error "dummy exception"))
          <*> o .: "output"
          <*> o .: "failing-case"
          <*> o .: "failing-labels"
          <*> o .: "failing-classes"
      _ -> fail "Unknown result type"

instance ToJSON Result where
  toJSON r = case r of
    Success {..} ->
      object
        [ "type" .= ("success" :: Text),
          "num-tests" .= numTests,
          "num-discarded" .= numDiscarded,
          "labels" .= labels,
          "classes" .= classes,
          "tables" .= tables,
          "output" .= output
        ]
    GaveUp {..} ->
      object
        [ "type" .= ("gave-up" :: Text),
          "num-tests" .= numTests,
          "num-discarded" .= numDiscarded,
          "labels" .= labels,
          "classes" .= classes,
          "tables" .= tables,
          "output" .= output
        ]
    Failure {..} ->
      object
        [ "type" .= ("failure" :: Text),
          "num-tests" .= numTests,
          "num-discarded" .= numDiscarded,
          "num-shrinks" .= numShrinks,
          "num-shrink-tries" .= numShrinkTries,
          "num-shrink-final" .= numShrinkFinal,
          "seed" .= (), -- Dummy
          "size" .= usedSize,
          "reason" .= reason,
          "exception" .= (), -- Dummy
          "output" .= output,
          "failing-case" .= failingTestCase,
          "failing-labels" .= failingLabels,
          "failing-classes" .= failingClasses
        ]

runPropertyTest :: Property -> IO TestRunResult
runPropertyTest p = do
  let args = stdArgs -- Customise args
  result <- runInSilencedNiceProcess $ quickCheckWithResult args p
  testRunResultStatus <- pure $ case result of
    Success {} -> TestPassed
    GaveUp {} -> TestFailed
    Failure {} -> TestFailed
    NoExpectedFailure {} -> TestFailed
  -- InsufficientCoverage {} -> TestFailed
  pure TestRunResult {..}

pureExceptionHandlers :: a -> [Handler a]
pureExceptionHandlers a =
  [ Handler $ \(_ :: ErrorCall) -> pure a,
    Handler $ \(_ :: RecConError) -> pure a,
    Handler $ \(_ :: RecSelError) -> pure a,
    Handler $ \(_ :: RecUpdError) -> pure a,
    Handler $ \(_ :: PatternMatchFail) -> pure a
  ]

type Test = IO ()

data TestRunResult
  = TestRunResult
      { testRunResultStatus :: TestStatus
      }
  deriving (Show, Generic)

instance FromJSON TestRunResult

instance ToJSON TestRunResult

data TestStatus = TestPassed | TestFailed
  deriving (Show, Eq, Generic)

instance FromJSON TestStatus

instance ToJSON TestStatus

runInSilencedNiceProcess :: (FromJSON a, ToJSON a) => IO a -> IO a
runInSilencedNiceProcess func = do
  -- FIXME: Forking a process is expensive and probably not needed for pure code.
  (sharedMemOutsideFd, sharedMemInsideFd) <- createPipe
  sharedMemOutsideHandle <- fdToHandle sharedMemOutsideFd
  sharedMemInsideHandle <- fdToHandle sharedMemInsideFd
  testProcess <- forkProcess $ do
    -- Be nice while testing so that the computer cannot lock up.
    nice 19
    -- Don't input or output anything
    newStdin <- createFile "/dev/null" ownerModes
    _ <- dupTo newStdin stdInput
    newStdout <- createFile "/dev/null" ownerModes
    _ <- dupTo newStdout stdOutput
    newStderr <- createFile "/dev/null" ownerModes
    _ <- dupTo newStderr stdError
    -- Actually run the function
    status <- func
    -- Encode the output
    let statusBs = LB.toStrict $ JSON.encode status
    -- Write it to the pipe
    SB.hPut sharedMemInsideHandle statusBs
    hFlush sharedMemInsideHandle
    hClose sharedMemInsideHandle
  -- Wait for the testing process to finish
  _ <- getProcessStatus True False testProcess
  -- Read its result from the pipe
  statusBs <- LB.fromStrict <$> SB.hGetSome sharedMemOutsideHandle 10240 -- FIXMe figure out a way to get rid of this magic constant
  case JSON.eitherDecode statusBs of
    Left err -> die err -- This cannot happen if the result was fewer than 1024 bytes in size
    Right res -> pure res

shouldBe :: Eq a => a -> a -> IO ()
shouldBe actual expected = unless (actual == expected) $ error "Not equal"
