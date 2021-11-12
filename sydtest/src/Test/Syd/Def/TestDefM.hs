{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Def.TestDefM where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Random
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Kind
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import System.Random.Shuffle
import Test.QuickCheck.IO ()
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.SpecDef

-- | A synonym for easy migration from hspec
type Spec = SpecWith ()

-- | A synonym for easy migration from hspec
type SpecWith inner = SpecM inner ()

-- | A synonym for easy migration from hspec
type SpecM inner result = TestDefM '[] inner result

-- | A synonym for a test suite definition
type TestDef outers inner = TestDefM outers inner ()

-- | The test definition monad
--
-- This type has three parameters:
--
-- * @outers@: A type-level list of the outer resources. These are resources that are prived once, around a group of tests. (This is the type of the results of `aroundAll`.)
-- * @inner@: The inner resource. This is a resource that is set up around every test, and even every example of a property test. (This is the type of the result of `around`.)
-- * @result@: The result ('TestDefM' is a monad.)
--
-- In practice, all of these three parameters should be '()' at the top level.
newtype TestDefM (outers :: [Type]) inner result = TestDefM
  { unTestDefM :: RWST TestRunSettings (TestForest outers inner) () IO result
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestRunSettings, MonadWriter (TestForest outers inner), MonadState ())

execTestDefM :: Settings -> TestDefM outers inner result -> IO (TestForest outers inner)
execTestDefM sets = fmap snd . runTestDefM sets

runTestDefM :: Settings -> TestDefM outers inner result -> IO (result, TestForest outers inner)
runTestDefM sets defFunc = do
  let func = unTestDefM defFunc
  (a, _, testForest) <- runRWST func (toTestRunSettings sets) ()
  let testForest' = filterTestForest (settingFilter sets) testForest
  stdgen <- case settingSeed sets of
    FixedSeed seed -> pure $ mkStdGen seed
    RandomSeed -> newStdGen
  let testForest'' =
        if settingRandomiseExecutionOrder sets
          then evalRand (randomiseTestForest testForest') stdgen
          else testForest'
  pure (a, testForest'')

toTestRunSettings :: Settings -> TestRunSettings
toTestRunSettings Settings {..} =
  TestRunSettings
    { testRunSettingSeed = settingSeed,
      testRunSettingMaxSuccess = settingMaxSuccess,
      testRunSettingMaxSize = settingMaxSize,
      testRunSettingMaxDiscardRatio = settingMaxDiscard,
      testRunSettingMaxShrinks = settingMaxShrinks,
      testRunSettingGoldenStart = settingGoldenStart,
      testRunSettingGoldenReset = settingGoldenReset
    }

filterTestForest :: Maybe Text -> SpecDefForest outers inner result -> SpecDefForest outers inner result
filterTestForest mf = fromMaybe [] . goForest DList.empty
  where
    goForest :: DList Text -> SpecDefForest a b c -> Maybe (SpecDefForest a b c)
    goForest ts sdf = do
      let sdf' = mapMaybe (goTree ts) sdf
      guard $ not $ null sdf'
      pure sdf'

    filterGuard :: DList Text -> Bool
    filterGuard dl = case mf of
      Just f -> f `T.isInfixOf` T.intercalate "." (DList.toList dl)
      Nothing -> True

    goTree :: DList Text -> SpecDefTree a b c -> Maybe (SpecDefTree a b c)
    goTree dl = \case
      DefSpecifyNode t td e -> do
        let tl = DList.snoc dl t
        guard $ filterGuard tl
        pure $ DefSpecifyNode t td e
      DefPendingNode t mr -> do
        let tl = DList.snoc dl t
        guard $ filterGuard tl
        pure $ DefPendingNode t mr
      DefDescribeNode t sdf -> DefDescribeNode t <$> goForest (DList.snoc dl t) sdf
      DefWrapNode func sdf -> DefWrapNode func <$> goForest dl sdf
      DefBeforeAllNode func sdf -> DefBeforeAllNode func <$> goForest dl sdf
      DefAroundAllNode func sdf -> DefAroundAllNode func <$> goForest dl sdf
      DefAroundAllWithNode func sdf -> DefAroundAllWithNode func <$> goForest dl sdf
      DefAfterAllNode func sdf -> DefAfterAllNode func <$> goForest dl sdf
      DefParallelismNode func sdf -> DefParallelismNode func <$> goForest dl sdf
      DefRandomisationNode func sdf -> DefRandomisationNode func <$> goForest dl sdf
      DefFlakinessNode func sdf -> DefFlakinessNode func <$> goForest dl sdf

randomiseTestForest :: MonadRandom m => SpecDefForest outers inner result -> m (SpecDefForest outers inner result)
randomiseTestForest = goForest
  where
    goForest :: MonadRandom m => SpecDefForest a b c -> m (SpecDefForest a b c)
    goForest = traverse goTree >=> shuffleM
    goTree :: MonadRandom m => SpecDefTree a b c -> m (SpecDefTree a b c)
    goTree = \case
      DefSpecifyNode t td e -> pure $ DefSpecifyNode t td e
      DefPendingNode t mr -> pure $ DefPendingNode t mr
      DefDescribeNode t sdf -> DefDescribeNode t <$> goForest sdf
      DefWrapNode func sdf -> DefWrapNode func <$> goForest sdf
      DefBeforeAllNode func sdf -> DefBeforeAllNode func <$> goForest sdf
      DefAroundAllNode func sdf -> DefAroundAllNode func <$> goForest sdf
      DefAroundAllWithNode func sdf -> DefAroundAllWithNode func <$> goForest sdf
      DefAfterAllNode func sdf -> DefAfterAllNode func <$> goForest sdf
      DefParallelismNode func sdf -> DefParallelismNode func <$> goForest sdf
      DefFlakinessNode i sdf -> DefFlakinessNode i <$> goForest sdf
      DefRandomisationNode eor sdf ->
        DefRandomisationNode eor <$> case eor of
          RandomiseExecutionOrder -> goForest sdf
          DoNotRandomiseExecutionOrder -> pure sdf
