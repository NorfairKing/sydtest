{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Def where

import Control.Monad.RWS.Strict
import qualified Data.Text as T
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Test.Syd.SpecForest
import UnliftIO
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
  let testDef =
        TestDef
          { testDefVal = \supplyArg -> runTest sets (\func -> supplyArg func) t,
            testDefCallStack = callStack
          }
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

-- | Run a custom action before every spec item.
before :: IO a -> SpecWith a -> Spec
before action = around (action >>=)

-- | Run a custom action before every spec item.
before_ :: IO () -> SpecWith a -> SpecWith a
before_ action = around_ (action >>)

-- | Run a custom action after every spec item.
after :: (a -> IO ()) -> SpecWith a -> SpecWith a
after action = aroundWith $ \e x -> e x `finally` action x

-- | Run a custom action after every spec item.
after_ :: IO () -> SpecWith a -> SpecWith a
after_ action = after $ \_ -> action

-- | Run a custom action before and/or after every spec item.
around :: ((a -> IO ()) -> IO ()) -> SpecWith a -> Spec
around action = aroundWith $ \e () -> action e

-- | Run a custom action before and/or after every spec item.
around_ :: (IO () -> IO ()) -> SpecWith a -> SpecWith a
around_ action = aroundWith $ \e a -> action (e a)

aroundWith :: forall a b r. ((a -> IO ()) -> (b -> IO ())) -> TestDefM a r -> TestDefM b r
aroundWith func (TestDefM rwst) = TestDefM $ flip mapRWST rwst $ \inner -> do
  (res, s, forest) <- inner
  let modifyVal ::
        (((a -> IO ()) -> IO ()) -> ResourceT IO TestRunResult) -> (((b -> IO ()) -> IO ()) -> ResourceT IO TestRunResult)
      modifyVal takeSupplyA supplyB =
        let supplyA :: (a -> IO ()) -> IO ()
            supplyA takeA = supplyB $ func takeA
         in takeSupplyA supplyA
  let forest' :: TestForest b
      forest' = fmap (fmap (fmap modifyVal)) forest
  pure (res, s, forest')

data TestDef a = TestDef {testDefVal :: a, testDefCallStack :: CallStack}
  deriving (Functor, Foldable, Traversable)

type TestForest a = SpecForest (TestDef (((a -> IO ()) -> IO ()) -> ResourceT IO TestRunResult))

type TestTree a = SpecTree (TestDef (((a -> IO ()) -> IO ()) -> ResourceT IO TestRunResult))

type ResultForest = SpecForest (TestDef TestRunResult)

type ResultTree = SpecTree (TestDef TestRunResult)
