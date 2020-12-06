{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.TimingSpec (spec) where

import Control.Concurrent
import System.IO.Unsafe
import Test.QuickCheck
import Test.Syd

spec :: Spec
spec = do
  it "takes at least 100 milliseconds (pure)" $
    unsafePerformIO take100ms `seq` True
  it "takes at least 100 milliseconds (IO)" $ do
    threadDelay 100_000
  it "takes at least 100 milliseconds (property) " $
    property $ \() -> do
      threadDelay 1_000

{-# NOINLINE take100ms #-}
take100ms :: IO ()
take100ms = threadDelay 100_000
