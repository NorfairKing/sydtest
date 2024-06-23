module Baz.Qux.QuxSpec where

import Test.Syd

spec :: Spec
spec = do
  describe "several nesting levels" $
    it "works" True
