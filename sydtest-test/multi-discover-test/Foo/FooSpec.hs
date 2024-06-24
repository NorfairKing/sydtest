module Foo.FooSpec where

import Test.Syd

spec :: Spec
spec = do
  describe "plain test" $
    it "simple example" $
      2 + 3 == (5 :: Int)
