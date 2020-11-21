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
type Spec = SpecWith () ()

type SpecWith a b = SpecM a b ()

type SpecM a b c = TestDefM a b c

newtype TestDefM a b c
  = TestDefM
      { unTestDefM :: RWST TestRunSettings (TestForest a b) () IO c
      }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestRunSettings, MonadWriter (TestForest a b), MonadState ())

execTestDefM :: TestDefM a b c -> IO (TestForest a b)
execTestDefM = fmap snd . runTestDefM

runTestDefM :: TestDefM a b c -> IO (c, TestForest a b)
runTestDefM defFunc = do
  let func = unTestDefM defFunc
  (a, _, testForest) <- runRWST func defaultSettings () -- TODO allow passing in settings from the command-line
  pure (a, testForest)

describe :: String -> TestDefM a b c -> TestDefM a b c
describe s func = censor ((: []) . DefDescribeNode (T.pack s)) func

it :: (HasCallStack, IsTest test) => String -> test -> TestDefM (Arg1 test) (Arg2 test) ()
it s t = do
  sets <- ask
  let testDef =
        TestDef
          { testDefVal = \supplyArgs -> runTest sets (\func -> supplyArgs func) t,
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

modifyRunSettings :: (TestRunSettings -> TestRunSettings) -> TestDefM a b c -> TestDefM a b c
modifyRunSettings = local

modifyMaxSuccess :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxSuccess func = modifyRunSettings $ \trs -> trs {testRunSettingMaxSuccess = func (testRunSettingMaxSuccess trs)}

modifyMaxDiscardRatio :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxDiscardRatio func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

modifyMaxSize :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxSize func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

modifyMaxShrinks :: (Int -> Int) -> TestDefM a b c -> TestDefM a b c
modifyMaxShrinks func = modifyRunSettings $ \trs -> trs {testRunSettingMaxDiscardRatio = func (testRunSettingMaxDiscardRatio trs)}

-- | Run a custom action before every spec item.
before :: IO c -> TestDefM a c e -> TestDefM a () e
before action = around (action >>=)

-- | Run a custom action before every spec item.
before_ :: IO () -> TestDefM a c e -> TestDefM a c e
before_ action = around_ (action >>)

-- | Run a custom action after every spec item.
after :: (c -> IO ()) -> TestDefM a c e -> TestDefM a c e
after action = aroundWith $ \e x -> e x `finally` action x

-- | Run a custom action after every spec item.
after_ :: IO () -> TestDefM a c e -> TestDefM a c e
after_ action = after $ \_ -> action

-- | Run a custom action before and/or after every spec item.
around :: ((c -> IO ()) -> IO ()) -> TestDefM a c e -> TestDefM a () e
around action = aroundWith $ \e () -> action e

-- | Run a custom action before and/or after every spec item.
around_ :: (IO () -> IO ()) -> TestDefM a c e -> TestDefM a c e
around_ action = aroundWith $ \e a -> action (e a)

aroundWith :: forall a c d r. ((c -> IO ()) -> (d -> IO ())) -> TestDefM a c r -> TestDefM a d r
aroundWith func (TestDefM rwst) = TestDefM $ flip mapRWST rwst $ \inner -> do
  (res, s, forest) <- inner
  let modifyVal ::
        forall x.
        (((x -> c -> IO ()) -> IO ()) -> ResourceT IO TestRunResult) ->
        (((x -> d -> IO ()) -> IO ()) -> ResourceT IO TestRunResult)
      modifyVal takeSupplyA supplyB =
        let supplyA :: (x -> c -> IO ()) -> IO ()
            supplyA takeA = supplyB $ \x -> func (takeA x)
         in takeSupplyA supplyA
      modifyTree :: forall x e. SpecDefTree x c e -> SpecDefTree x d e
      modifyTree = \case
        DefDescribeNode t sdf -> DefDescribeNode t $ modifyForest sdf
        DefSpecifyNode t td e -> DefSpecifyNode t (modifyVal <$> td) e
        DefAroundAllNode f sdf -> DefAroundAllNode f $ modifyForest sdf
      modifyForest :: forall x e. SpecDefForest x c e -> SpecDefForest x d e
      modifyForest = map modifyTree
  let forest' :: SpecDefForest a d ()
      forest' = modifyForest forest
  pure (res, s, forest')

-- | Run a custom action before all spec items.
beforeAll :: ResourceT IO a -> TestDefM a b e -> TestDefM () b e
beforeAll action = aroundAll (action >>=)

-- | Run a custom action before all spec items.
beforeAll_ :: ResourceT IO () -> TestDefM a b e -> TestDefM a b e
beforeAll_ action = aroundAll_ (action >>)

-- | Run a custom action after all spec items.
afterAll :: (a -> ResourceT IO ()) -> TestDefM a b e -> TestDefM a b e
afterAll action = aroundAllWith $ \e x -> e x `finally` action x

-- | Run a custom action after all spec items.
afterAll_ :: ResourceT IO () -> TestDefM a b e -> TestDefM a b e
afterAll_ action = afterAll $ \_ -> action

-- | Run a custom action before and/or after all spec items.
aroundAll :: ((a -> ResourceT IO ()) -> ResourceT IO ()) -> TestDefM a b e -> TestDefM () b e
aroundAll action = aroundAllWith $ \e () -> action e

-- | Run a custom action before and/or after all spec items.
aroundAll_ :: (ResourceT IO () -> ResourceT IO ()) -> TestDefM a b e -> TestDefM a b e
aroundAll_ action = aroundAllWith $ \e a -> action (e a)

aroundAllWith :: forall a b c r. ((a -> ResourceT IO ()) -> (b -> ResourceT IO ())) -> TestDefM a c r -> TestDefM b c r
aroundAllWith func (TestDefM rwst) = TestDefM $ flip mapRWST rwst $ \inner -> do
  (res, s, forest) <- inner
  let forest' = [DefAroundAllNode func forest]
  pure (res, s, forest')

data TestDef v = TestDef {testDefVal :: v, testDefCallStack :: CallStack}
  deriving (Functor, Foldable, Traversable)

type TestForest a c = SpecDefForest a c ()

type TestTree a c = SpecDefTree a c ()

type SpecDefForest a c e = [SpecDefTree a c e]

data SpecDefTree a c e where -- a: input from 'aroundAll', c: input from 'around', e: extra
  DefDescribeNode :: Text -> SpecDefForest a c e -> SpecDefTree a c e -- A description
  DefSpecifyNode :: Text -> TestDef (((a -> c -> IO ()) -> IO ()) -> ResourceT IO TestRunResult) -> e -> SpecDefTree a c e -- A test with its description
  DefAroundAllNode :: ((a -> ResourceT IO ()) -> (b -> ResourceT IO ())) -> SpecDefForest a c e -> SpecDefTree b c e

instance Functor (SpecDefTree a c) where
  fmap f = \case
    DefDescribeNode t sf -> DefDescribeNode t (map (fmap f) sf)
    DefSpecifyNode t td e -> DefSpecifyNode t td (f e)
    DefAroundAllNode func sdf -> DefAroundAllNode func (map (fmap f) sdf)

instance Foldable (SpecDefTree a c) where
  foldMap f = \case
    DefDescribeNode _ sf -> foldMap (foldMap f) sf
    DefSpecifyNode _ _ e -> f e
    DefAroundAllNode _ sdf -> foldMap (foldMap f) sdf

instance Traversable (SpecDefTree a c) where
  traverse f = \case
    DefDescribeNode t sdf -> DefDescribeNode t <$> traverse (traverse f) sdf
    DefSpecifyNode t td e -> DefSpecifyNode t td <$> f e
    DefAroundAllNode func sdf -> DefAroundAllNode func <$> traverse (traverse f) sdf

type ResultForest = SpecForest (TestDef TestRunResult)

type ResultTree = SpecTree (TestDef TestRunResult)
