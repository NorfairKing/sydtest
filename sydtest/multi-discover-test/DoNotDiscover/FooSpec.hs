{-# LANGUAGE TypeApplications #-}

module DoNotDiscover.FooSpec where

import Test.Syd

spec :: Spec
spec = do
  describe "do not discover" $
    it "always fails" $
      expectationFailure @() "This test fails because sydtest-discover isn't working properly. This spec should not be discovered"
