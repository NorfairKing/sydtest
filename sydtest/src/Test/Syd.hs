{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd where

import Control.Exception
import Control.Monad.Reader
import Data.Aeson as JSON
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.IORef
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Rainbow
import System.Exit
import System.IO (hClose, hFlush)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process

sydTest :: Spec -> IO ()
sydTest spec = do
  ((), specForest) <- runTestDefM spec
  resultForest <- runSpecForest specForest
  printOutputSpecForest resultForest
  when (shouldExitFail resultForest) (exitWith (ExitFailure 1))

type Spec = TestDefM ()

data TestDefEnv
  = TestDefEnv
      { testDefEnvForest :: IORef (SpecForest Test)
      }

newtype TestDefM a = TestDefM {unTestDefM :: ReaderT TestDefEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestDefEnv)

runTestDefM :: TestDefM a -> IO (a, SpecForest Test)
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

it :: String -> Test -> TestDefM ()
it s t = do
  var <- asks testDefEnvForest
  liftIO $ modifyIORef var $ (++ [SpecifyNode s t]) -- FIXME this can probably be slow

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

runSpecForest :: SpecForest Test -> IO (SpecForest TestRunResult)
runSpecForest = traverse (traverse runTest)

runSpecTree :: SpecTree Test -> IO (SpecTree TestRunResult)
runSpecTree = traverse runTest

printOutputSpecForest :: SpecForest TestRunResult -> IO ()
printOutputSpecForest results = do
  byteStringMaker <- byteStringMakerFromEnvironment
  let bytestrings = map (chunksToByteStrings byteStringMaker) (outputSpecForest results) :: [[ByteString]]
  forM_ bytestrings $ \bs -> do
    mapM_ SB.putStr bs
    SB8.putStrLn ""

outputSpecForest :: SpecForest TestRunResult -> [[Chunk]]
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

type Test = IO ()

data TestRunResult = TestRunResult {testRunResultStatus :: TestStatus}
  deriving (Show)

data TestStatus = TestPassed | TestFailed
  deriving (Show, Eq, Generic)

instance FromJSON TestStatus

instance ToJSON TestStatus

runTest :: Test -> IO TestRunResult
runTest func = do
  testRunResultStatus <-
    runInSilencedNiceProcess $
      (func >>= (evaluate . (`seq` TestPassed)))
        `catches` [ Handler $ \(_ :: ErrorCall) -> pure TestFailed,
                    Handler $ \(_ :: ExitCode) -> pure TestFailed,
                    Handler $ \(_ :: RecConError) -> pure TestFailed,
                    Handler $ \(_ :: RecSelError) -> pure TestFailed,
                    Handler $ \(_ :: RecUpdError) -> pure TestFailed,
                    Handler $ \(_ :: PatternMatchFail) -> pure TestFailed
                  ]
  pure TestRunResult {..}

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
  statusBs <- LB.fromStrict <$> SB.hGetSome sharedMemOutsideHandle 1024 -- FIXMe figure out a way to get rid of this magic constant
  case JSON.eitherDecode statusBs of
    Left err -> die err -- This cannot happen if the result was fewer than 1024 bytes in size
    Right res -> pure res
