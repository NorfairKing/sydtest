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
import Data.Foldable
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.Stack
import Rainbow
import Safe
import System.Exit
import System.IO (hClose, hFlush)
import System.Posix.Files
import System.Posix.IO
import System.Posix.Process
import System.TimeIt
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.IO ()
import Test.QuickCheck.Test
import Text.Printf

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
  liftIO $ modifyIORef var $ (++ [DescribeNode (T.pack s) sf]) -- FIXME this can probably be slow because of ++
  pure a

it :: (HasCallStack, IsTest test) => String -> test -> TestDefM ()
it s t = do
  var <- asks testDefEnvForest
  let testDef = TestDef {testDefVal = runTest t, testDefCallStack = callStack}
  liftIO $ modifyIORef var $ (++ [SpecifyNode (T.pack s) testDef]) -- FIXME this can probably be slow because of ++

type SpecForest a = [SpecTree a]

data SpecTree a
  = DescribeNode Text (SpecForest a) -- A description
  | SpecifyNode Text a -- A test with its description
  deriving (Show, Functor)

instance Foldable SpecTree where
  foldMap f = \case
    DescribeNode _ sts -> foldMap (foldMap f) sts
    SpecifyNode _ a -> f a

instance Traversable SpecTree where
  traverse func = \case
    DescribeNode s sts -> DescribeNode s <$> traverse (traverse func) sts
    SpecifyNode s a -> SpecifyNode s <$> func a

data TestDef a = TestDef {testDefVal :: a, testDefCallStack :: CallStack}
  deriving (Functor)

type TestForest = SpecForest (TestDef (IO TestRunResult))

type ResultForest = SpecForest (TestDef TestRunResult)

type ResultTree = SpecTree (TestDef TestRunResult)

runSpecForest :: TestForest -> IO ResultForest
runSpecForest = traverse $ traverse $ \td -> do
  let runTest = testDefVal td
  result <- runTest
  pure $ td {testDefVal = result}

printOutputSpecForest :: ResultForest -> IO ()
printOutputSpecForest results = do
  byteStringMaker <- byteStringMakerFromEnvironment
  let bytestrings = map (chunksToByteStrings byteStringMaker) (outputResultReport results) :: [[ByteString]]
  forM_ bytestrings $ \bs -> do
    mapM_ SB.putStr bs
    SB8.putStrLn ""

outputResultReport :: ResultForest -> [[Chunk]]
outputResultReport rf =
  concat
    [ [ [fore blue $ chunk "Tests:"],
        [chunk ""]
      ],
      outputSpecForest rf,
      [ [chunk ""],
        [chunk ""],
        [fore blue $ chunk "Failures:"],
        [chunk ""]
      ],
      outputFailures rf
    ]

outputSpecForest :: ResultForest -> [[Chunk]]
outputSpecForest = concatMap outputSpecTree

outputSpecTree :: ResultTree -> [[Chunk]]
outputSpecTree = \case
  DescribeNode t sf -> [fore yellow $ chunk t] : map (chunk "  " :) (outputSpecForest sf)
  SpecifyNode t (TestDef (TestRunResult {..}) _) ->
    map (map (fore (statusColour testRunResultStatus))) $
      filter
        (not . null)
        [ [ chunk (statusCheckMark testRunResultStatus),
            chunk t
          ],
          concat
            [ -- [chunk (T.pack (printf "%10.2f ms " (testRunResultExecutionTime * 1000)))],
              [chunk (T.pack (printf "  (passed for all of %d inputs)" w)) | w <- maybeToList testRunResultNumTests, testRunResultStatus == TestPassed]
            ]
        ]

outputFailures :: ResultForest -> [[Chunk]]
outputFailures rf =
  let failures = filter ((== TestFailed) . testRunResultStatus . testDefVal . snd) $ flattenSpecForest rf
      nbDigitsInFailureCount = ceiling $ logBase 10 $ genericLength failures :: Int
   in concat
        [ map (chunk "  " :) $ concat $ indexed failures $ \w (ts, TestDef (TestRunResult {..}) cs) ->
            [ [ (fore cyan) $ chunk $ T.pack $
                  case headMay $ getCallStack cs of
                    Nothing -> "Unknown location"
                    Just (_, SrcLoc {..}) ->
                      concat
                        [ srcLocFile,
                          ":",
                          show srcLocStartLine
                        ]
              ],
              map
                (fore (statusColour testRunResultStatus))
                [ chunk $ statusCheckMark testRunResultStatus,
                  chunk $ T.pack (printf ("%" ++ show nbDigitsInFailureCount ++ "d ") w),
                  chunk $ T.intercalate "." ts
                ],
              [chunk ""]
            ]
        ]

indexed :: [a] -> (Word -> a -> b) -> [b]
indexed ls func = zipWith func [1 ..] ls

flattenSpecForest :: SpecForest a -> [([Text], a)]
flattenSpecForest = concatMap flattenSpecTree

flattenSpecTree :: SpecTree a -> [([Text], a)]
flattenSpecTree = \case
  DescribeNode t sf -> map (\(ts, a) -> (t : ts, a)) $ flattenSpecForest sf
  SpecifyNode t a -> [([t], a)]

outputFailure :: TestRunResult -> Maybe [[Chunk]]
outputFailure TestRunResult {..} = case testRunResultStatus of
  TestPassed -> Nothing
  TestFailed -> Just [[chunk "Failure"]]

statusColour :: TestStatus -> Radiant
statusColour = \case
  TestPassed -> green
  TestFailed -> red

statusCheckMark :: TestStatus -> Text
statusCheckMark = \case
  TestPassed -> "\10003 "
  TestFailed -> "\10007 "

shouldExitFail :: ResultForest -> Bool
shouldExitFail = any (any ((== TestFailed) . testRunResultStatus . testDefVal))

class IsTest a where
  runTest :: a -> IO TestRunResult

instance IsTest Bool where
  runTest = runPureTest

runPureTest :: Bool -> IO TestRunResult
runPureTest b = do
  let testRunResultNumTests = Nothing
  (testRunResultExecutionTime, resultBool) <- timeItT $ (evaluate b) `catches` (pureExceptionHandlers False)
  let testRunResultStatus = if resultBool then TestPassed else TestFailed
  pure TestRunResult {..}

instance IsTest (IO a) where
  runTest = runIOTest

runIOTest :: IO a -> IO TestRunResult
runIOTest func = do
  let testRunResultNumTests = Nothing
  (testRunResultExecutionTime, testRunResultStatus) <-
    timeItT
      $ runInSilencedNiceProcess
      $ (func >>= (evaluate . (`seq` TestPassed)))
        `catches` ( [ Handler $ \(_ :: ExitCode) -> pure TestFailed
                    ]
                      ++ (pureExceptionHandlers TestFailed)
                  )
  pure TestRunResult {..}

instance IsTest Property where
  runTest = runPropertyTest

deriving instance Generic Result

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
      { testRunResultStatus :: !TestStatus,
        testRunResultExecutionTime :: !Double,
        testRunResultNumTests :: !(Maybe Word)
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
  statusBs <- LB.fromStrict <$> SB.hGetSome sharedMemOutsideHandle 10240 -- FIXME figure out a way to get rid of this magic constant
  case JSON.eitherDecode statusBs of
    Left err -> die err -- This cannot happen if the result was fewer than 1024 bytes in size
    Right res -> pure res

shouldBe :: Eq a => a -> a -> IO ()
shouldBe actual expected = unless (actual == expected) $ error "Not equal"
