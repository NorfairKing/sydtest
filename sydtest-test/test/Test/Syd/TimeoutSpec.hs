{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.TimeoutSpec (spec) where

import Control.Concurrent
import Test.Syd

spec :: Spec
spec = do
  expectFailing $
    withTimeout 100_000 $ do
      it "times out after 100 ms" $
        threadDelay 200_000
      expectPassing $
        withoutTimeout $
          it "does not timeout after 100 ms" $
            threadDelay 200_000
