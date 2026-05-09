{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Def.TestDefM where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Kind
import Data.Text (Text)
import GHC.Generics (Generic)
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
  { unTestDefM :: WriterT (TestForest outers inner) (ReaderT TestDefEnv IO) result
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader TestDefEnv,
      MonadWriter (TestForest outers inner)
    )

data TestDefEnv = TestDefEnv
  { testDefEnvDescriptionPath :: ![Text],
    testDefEnvTestRunSettings :: !TestRunSettings
  }
  deriving (Show, Eq, Generic)

execTestDefM :: Settings -> TestDefM outers inner result -> IO (TestForest outers inner)
execTestDefM sets = fmap snd . runTestDefM sets

runTestDefM :: Settings -> TestDefM outers inner result -> IO (result, TestForest outers inner)
runTestDefM sets defFunc = do
  let func = unTestDefM defFunc
  let testDefEnv =
        TestDefEnv
          { testDefEnvDescriptionPath = [],
            testDefEnvTestRunSettings = toTestRunSettings sets
          }
  (a, testForest) <- runReaderT (runWriterT func) testDefEnv
  let testForest' = filterTestForest (settingFilters sets) testForest
  stdgen <- case settingSeed sets of
    FixedSeed seed -> pure $ mkStdGen seed
    RandomSeed -> newStdGen
  let testForest'' =
        if settingRandomiseExecutionOrder sets
          then evalRand (randomiseTestForest testForest') stdgen
          else testForest'
  pure (a, testForest'')

-- | Get the path of 'describe' strings upwards.
--
-- Note that using this function makes tests less movable, depending on what
-- you do with these strings.
-- For example, if you use these strings to define the path to a golden test
-- file, then that path will change if you move the tests somewhere else.
-- This combines unfortunately with the way @sydtest-discover@ makes the module
-- name part of this path.
-- Indeed: moving your tests to another module will change their path as well,
-- if you use @sydtest-discover@.
-- Also note that while test forests can be randomised, their description path
-- upwards will not, because of how trees are structured.
getTestDescriptionPath :: TestDefM outers inner [Text]
getTestDescriptionPath = asks testDefEnvDescriptionPath

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
