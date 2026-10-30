module Test.Syd.MutationMode.SingleSpec (spec) where

import Control.Monad (void)
import Data.IORef
import Test.QuickCheck
import Test.Syd
import Test.Syd.MutationMode.Single (mutationChildSettings, mutationChildSpecForest)
import Test.Syd.OptParse

spec :: Spec
spec = do
  describe "mutationChildSpecForest" $
    -- A test that shrank its failing input would be run again for every
    -- shrink step, and once more for every retry.
    it "runs a failing property once, without shrinking or retrying it" $ do
      runs <- newIORef (0 :: Int)
      let failing :: Spec
          failing =
            it "fails" $
              forAllShrink (pure (100 :: Word)) shrink $ \_ ->
                ioProperty $ do
                  modifyIORef' runs succ
                  pure False
      forest <- mutationChildSpecForest defaultSettings failing
      void $ runSpecForestSynchronously (mutationChildSettings defaultSettings) forest
      readIORef runs `shouldReturn` 1

  describe "mutationChildSettings" $
    it "runs one test at a time, stops at the first failure, and neither shrinks nor retries it" $
      mutationChildSettings defaultSettings
        `shouldBe` defaultSettings
          { settingThreads = Synchronous,
            settingFailFast = True,
            settingMaxShrinks = 0,
            settingRetries = 0
          }
