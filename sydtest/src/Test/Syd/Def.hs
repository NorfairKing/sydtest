{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Def where

import Control.Monad.RWS.Strict
import Data.Text (Text)
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
describe s func = censor ((: []) . DefDescribeNode (T.pack s)) func

it :: (HasCallStack, IsTest test) => String -> test -> TestDefM (Arg test) ()
it s t = do
  sets <- ask
  let testDef =
        TestDef
          { testDefVal = \supplyArg -> runTest sets (\func -> supplyArg func) t,
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

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
      modifyTree :: SpecDefTree a e -> SpecDefTree b e
      modifyTree = \case
        DefDescribeNode t sdf -> DefDescribeNode t $ modifyForest sdf
        DefSpecifyNode t td e -> DefSpecifyNode t (modifyVal <$> td) e
      modifyForest :: SpecDefForest a e -> SpecDefForest b e
      modifyForest = map modifyTree
  let forest' :: TestForest b
      forest' = modifyForest forest
  pure (res, s, forest')

data TestDef a = TestDef {testDefVal :: a, testDefCallStack :: CallStack}
  deriving (Functor, Foldable, Traversable)

type TestForest a = SpecDefForest a ()

type TestTree a = SpecDefTree a ()

type SpecDefForest a e = [SpecDefTree a e]

data SpecDefTree a e where -- a: testInput, e: extra
  DefDescribeNode :: Text -> SpecDefForest a e -> SpecDefTree a e -- A description
  DefSpecifyNode :: Text -> TestDef (((a -> IO ()) -> IO ()) -> ResourceT IO TestRunResult) -> e -> SpecDefTree a e -- A test with its description
      -- DefAroundAllNode :: ((a -> IO ()) -> (b -> IO ())) -> SpecDefTree a e -> SpecDefTree b e
      --

instance Functor (SpecDefTree a) where
  fmap f = \case
    DefDescribeNode t sf -> DefDescribeNode t (map (fmap f) sf)
    DefSpecifyNode t td e -> DefSpecifyNode t td (f e)

instance Foldable (SpecDefTree a) where
  foldMap f = \case
    DefDescribeNode _ sf -> foldMap (foldMap f) sf
    DefSpecifyNode _ _ e -> f e

instance Traversable (SpecDefTree a) where
  traverse f = \case
    DefDescribeNode t sdf -> DefDescribeNode t <$> traverse (traverse f) sdf
    DefSpecifyNode t td e -> DefSpecifyNode t td <$> f e

type ResultForest = SpecForest (TestDef TestRunResult)

type ResultTree = SpecTree (TestDef TestRunResult)
