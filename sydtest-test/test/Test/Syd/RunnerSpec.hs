module Test.Syd.RunnerSpec (spec) where

import Control.Monad
import Data.IORef
import System.Random (randomIO)
import Test.Syd
import Test.Syd.OptParse

spec :: Spec
spec = do
  it "Can run an empty test suite with many runners." $ do
    let settings = defaultSettings {settingThreads = Asynchronous 1}
    let runs = 10000
    let simpleSpec :: Spec
        simpleSpec =
          it "is a simple assertion" $ do
            True `shouldBe` True
    specForest <- execTestDefM settings simpleSpec
    replicateM_ runs $ do
      void $ runSpecForestAsynchronously settings 1 specForest

  it "seeds the global pseudorandom generator so a FixedSeed run is reproducible" $ do
    -- Regression test: the spec-forest runners must seed the global
    -- System.Random generator from the SeedSetting, not only QuickCheck's
    -- replay seed.  A test that draws from the global generator directly
    -- (randomIO, newStdGen, ...) is otherwise non-reproducible across runs,
    -- which matters for the mutation and coverage children that run a forest
    -- without going through the top-level sydTest runner.
    ref <- newIORef []
    let settings = defaultSettings {settingSeed = FixedSeed 42}
        randomSpec :: Spec
        randomSpec =
          it "draws from the global generator" $ do
            x <- randomIO :: IO Int
            modifyIORef ref (x :)
    specForest <- execTestDefM settings randomSpec
    -- Two independent runs of the same forest: each must re-seed the global
    -- generator, so both draw the same first value.
    void $ runSpecForestSynchronously settings specForest
    void $ runSpecForestSynchronously settings specForest
    draws <- readIORef ref
    case draws of
      [a, b] -> a `shouldBe` b
      _ -> expectationFailure "expected exactly two recorded draws"
