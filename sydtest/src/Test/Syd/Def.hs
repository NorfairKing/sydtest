{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Syd.Def where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Text as T
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Test.Syd.SpecForest

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

data TestDef a = TestDef {testDefVal :: a, testDefCallStack :: CallStack}
  deriving (Functor)

type TestForest = SpecForest (TestDef (IO TestRunResult))

type TestTree = SpecTree (TestDef (IO TestRunResult))

type ResultForest = SpecForest (TestDef TestRunResult)

type ResultTree = SpecTree (TestDef TestRunResult)
