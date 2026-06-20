module Example.UnliftedLibSpec (spec) where

import Example.UnliftedLib
import Test.Syd

spec :: Spec
spec =
  describe "subInts" $
    -- The unlifted (@Int#@) sites are skipped by the instrumenter, so this
    -- module produces no mutations; the spec just exercises it (and the lifted
    -- @I# (...)@ wrapper) so the module compiles and runs under instrumentation.
    -- See 'Example.UnliftedLib'.
    it "subtracts the second argument from the first" $
      subInts 5 3 `shouldBe` 2
