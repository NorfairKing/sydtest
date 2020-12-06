{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.TimingSpec (spec) where

import Control.Concurrent
import Test.Syd

spec :: Spec
spec = do
  -- it "really really really really really really really really really really really long description" $ do
  --   threadDelay 100_000
  it "takes at least 100 milliseconds" $ do
    threadDelay 100_000
