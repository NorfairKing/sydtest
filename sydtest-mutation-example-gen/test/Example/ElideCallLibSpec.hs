module Example.ElideCallLibSpec (spec) where

import Example.ElideCallLib
import Test.Syd

spec :: Spec
spec = do
  describe "negateInt" $
    -- @negate x@ differs from @x@ for any non-zero input, killing the
    -- ElideCall mutant @negate x -> x@.
    it "negates its argument" $
      negateInt 3 `shouldBe` (-3)

  describe "absSucc" $ do
    -- A negative input makes @succ x@ negative, so the outer mutant
    -- @abs (succ x) -> succ x@ gives @-2@ instead of @2@, and the inner mutant
    -- @succ x -> x@ gives @abs (-3) = 3@ instead of @2@.  Both are killed.
    it "takes the successor then the absolute value of a negative input" $
      absSucc (-3) `shouldBe` 2
    it "takes the successor then the absolute value of a positive input" $
      absSucc 4 `shouldBe` 5

  describe "idInt" $
    it "returns its argument unchanged" $
      idInt 5 `shouldBe` 5

  describe "same" $
    it "returns its argument unchanged" $
      same 4 `shouldBe` 4

  describe "useSame" $
    it "returns its argument unchanged" $
      useSame 9 `shouldBe` 9
