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

-- The same naming as in hspec for easy migration
type Spec = SpecWith ()

type SpecWith a = SpecM a ()

type SpecM a b = TestDefM a b

newtype TestDefM a b
  = TestDefM
      { unTestDefM :: RWST TestRunSettings (TestForest a) () IO b
      }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestRunSettings, MonadWriter (TestForest a), MonadState ())

runTestDefM :: TestDefM a b -> IO (b, TestForest a)
runTestDefM defFunc = do
  let func = unTestDefM defFunc
  (a, _, testForest) <- runRWST func defaultSettings () -- TODO allow passing in settings from the command-line
  pure (a, testForest)

describe :: String -> TestDefM a b -> TestDefM a b
describe s func = censor ((: []) . DescribeNode (T.pack s)) func

it :: (HasCallStack, IsTest test) => String -> test -> TestDefM (Arg test) ()
it s t = do
  sets <- ask
  let testDef = TestDef {testDefVal = \arg -> runTest sets (\func -> func arg) t, testDefCallStack = callStack}
  tell [SpecifyNode (T.pack s) testDef]

modifyRunSettings :: (TestRunSettings -> TestRunSettings) -> TestDefM a b -> TestDefM a b
modifyRunSettings = local

modifyMaxSuccess :: (Int -> Int) -> TestDefM a b -> TestDefM a b
modifyMaxSuccess func = modifyRunSettings $ \trs -> trs {testRunSettingMaxSuccess = func (testRunSettingMaxSuccess trs)}

modifyMaxDiscardRatio :: (Int -> Int) -> TestDefM a b -> TestDefM a b
modifyMaxDiscardRatio func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

modifyMaxSize :: (Int -> Int) -> TestDefM a b -> TestDefM a b
modifyMaxSize func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

modifyMaxShrinks :: (Int -> Int) -> TestDefM a b -> TestDefM a b
modifyMaxShrinks func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

data TestDef a = TestDef {testDefVal :: a, testDefCallStack :: CallStack}
  deriving (Functor, Foldable, Traversable)

type TestForest a = SpecForest (TestDef (a -> ResourceT IO TestRunResult))

type TestTree a = SpecTree (TestDef (a -> ResourceT IO TestRunResult))

type ResultForest = SpecForest (TestDef TestRunResult)

type ResultTree = SpecTree (TestDef TestRunResult)
