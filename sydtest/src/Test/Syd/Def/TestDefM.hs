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
type SpecWith a = SpecM a ()

-- | A synonym for easy migration from hspec
type SpecM a b = TestDefM '[] a b

-- | The test definition monad
--
-- This type has three parameters:
--
-- * @a@: The type of the result of `aroundAll`
-- * @b@: The type of the result of `around`
-- * @c@: The result
--
-- In practice, all of these three parameters should be '()' at the top level.
newtype TestDefM a b c = TestDefM
  { unTestDefM :: RWST TestRunSettings (TestForest a b) () IO c
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestRunSettings, MonadWriter (TestForest a b), MonadState ())

execTestDefM :: Settings -> TestDefM a b c -> IO (TestForest a b)
execTestDefM sets = fmap snd . runTestDefM sets

runTestDefM :: Settings -> TestDefM a b c -> IO (c, TestForest a b)
runTestDefM sets defFunc = do
  let func = unTestDefM defFunc
  (a, _, testForest) <- runRWST func (toTestRunSettings sets) () -- TODO allow passing in settings from the command-line
  let testForest' = filterTestForest (settingFilter sets) testForest
  let testForest'' =
        if settingRandomiseExecutionOrder sets
          then evalRand (randomiseTestForest testForest') (mkStdGen (settingSeed sets))
          else testForest'
  pure (a, testForest'')

toTestRunSettings :: Settings -> TestRunSettings
toTestRunSettings Settings {..} =
  TestRunSettings
    { testRunSettingSeed = settingSeed,
      testRunSettingMaxSuccess = settingMaxSuccess,
      testRunSettingMaxSize = settingMaxSize,
      testRunSettingMaxDiscardRatio = settingMaxDiscard,
      testRunSettingMaxShrinks = settingMaxShrinks
    }

filterTestForest :: Maybe Text -> SpecDefForest a b c -> SpecDefForest a b c
filterTestForest mf = fromMaybe [] . goForest DList.empty
  where
    goForest :: DList Text -> SpecDefForest a b c -> Maybe (SpecDefForest a b c)
    goForest ts sdf = do
      let sdf' = mapMaybe (goTree ts) sdf
      guard $ not $ null sdf'
      pure sdf'
    goTree :: DList Text -> SpecDefTree a b c -> Maybe (SpecDefTree a b c)
    goTree dl = \case
      DefSpecifyNode t td e -> do
        let tl = DList.toList (DList.snoc dl t)
        guard $ case mf of
          Just f -> f `T.isInfixOf` T.intercalate "." tl
          Nothing -> True
        pure $ DefSpecifyNode t td e
      DefDescribeNode t sdf -> DefDescribeNode t <$> goForest (DList.snoc dl t) sdf
      DefWrapNode func sdf -> DefWrapNode func <$> goForest dl sdf
      DefBeforeAllNode func sdf -> DefBeforeAllNode func <$> goForest dl sdf
      DefAroundAllNode func sdf -> DefAroundAllNode func <$> goForest dl sdf
      DefAroundAllWithNode func sdf -> DefAroundAllWithNode func <$> goForest dl sdf
      DefAfterAllNode func sdf -> DefAfterAllNode func <$> goForest dl sdf
      DefParallelismNode func sdf -> DefParallelismNode func <$> goForest dl sdf
      DefRandomisationNode func sdf -> DefRandomisationNode func <$> goForest dl sdf

randomiseTestForest :: MonadRandom m => SpecDefForest a b c -> m (SpecDefForest a b c)
randomiseTestForest = goForest
  where
    goForest :: MonadRandom m => SpecDefForest a b c -> m (SpecDefForest a b c)
    goForest = traverse goTree >=> shuffleM
    goTree :: MonadRandom m => SpecDefTree a b c -> m (SpecDefTree a b c)
    goTree = \case
      DefSpecifyNode t td e -> pure $ DefSpecifyNode t td e
      DefDescribeNode t sdf -> DefDescribeNode t <$> goForest sdf
      DefWrapNode func sdf -> DefWrapNode func <$> goForest sdf
      DefBeforeAllNode func sdf -> DefBeforeAllNode func <$> goForest sdf
      DefAroundAllNode func sdf -> DefAroundAllNode func <$> goForest sdf
      DefAroundAllWithNode func sdf -> DefAroundAllWithNode func <$> goForest sdf
      DefAfterAllNode func sdf -> DefAfterAllNode func <$> goForest sdf
      DefParallelismNode func sdf -> DefParallelismNode func <$> goForest sdf
      DefRandomisationNode eor sdf ->
        DefRandomisationNode eor <$> case eor of
          RandomiseExecutionOrder -> goForest sdf
          DoNotRandomiseExecutionOrder -> pure sdf
