module Example.MaybeCompoundLibSpec (spec) where

import Example.MaybeCompoundLib
import Test.Syd

spec :: Spec
spec =
  describe "wrapPair" $
    -- Kills the MaybeOp mutant (@Just p -> Nothing@): with the mutation the
    -- result is Nothing instead of @Just (1, Just 2)@.  The compound element
    -- type @(Int, Maybe Int)@ is the regression trigger; see
    -- 'Example.MaybeCompoundLib'.
    it "wraps its argument in Just" $
      wrapPair (1, Just 2) `shouldBe` Just (1, Just 2)
