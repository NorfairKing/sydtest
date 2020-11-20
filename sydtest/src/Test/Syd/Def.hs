{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Syd.Def where

import Control.Monad.RWS.Strict
import qualified Data.Text as T
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Test.Syd.SpecForest
import UnliftIO.Resource

type Spec = TestDefM ()

newtype TestDefM a = TestDefM {unTestDefM :: RWST TestRunSettings TestForest () IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestRunSettings, MonadWriter TestForest, MonadState ())

runTestDefM :: TestDefM a -> IO (a, TestForest)
runTestDefM defFunc = do
  let func = unTestDefM defFunc
  (a, _, testForest) <- runRWST func defaultSettings () -- TODO allow passing in settings from the command-line
  pure (a, testForest)

describe :: String -> TestDefM a -> TestDefM a
describe s func = censor ((: []) . DescribeNode (T.pack s)) func

it :: (HasCallStack, IsTest test) => String -> test -> TestDefM ()
it s t = do
  sets <- ask
  let testDef = TestDef {testDefVal = runTest sets t, testDefCallStack = callStack}
  tell [SpecifyNode (T.pack s) testDef]

modifyRunSettings :: (TestRunSettings -> TestRunSettings) -> TestDefM a -> TestDefM a
modifyRunSettings = local

modifyMaxSuccess :: (Int -> Int) -> TestDefM a -> TestDefM a
modifyMaxSuccess func = modifyRunSettings $ \trs -> trs {testRunSettingMaxSuccess = func (testRunSettingMaxSuccess trs)}

modifyMaxDiscardRatio :: (Int -> Int) -> TestDefM a -> TestDefM a
modifyMaxDiscardRatio func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

modifyMaxSize :: (Int -> Int) -> TestDefM a -> TestDefM a
modifyMaxSize func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

modifyMaxShrinks :: (Int -> Int) -> TestDefM a -> TestDefM a
modifyMaxShrinks func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

data TestDef a = TestDef {testDefVal :: a, testDefCallStack :: CallStack}
  deriving (Functor, Foldable, Traversable)

type TestForest = SpecForest (TestDef (ResourceT IO TestRunResult))

type TestTree = SpecTree (TestDef (ResourceT IO TestRunResult))

type ResultForest = SpecForest (TestDef TestRunResult)

type ResultTree = SpecTree (TestDef TestRunResult)
