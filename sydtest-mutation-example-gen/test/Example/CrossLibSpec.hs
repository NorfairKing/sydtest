module Example.CrossLibSpec (spec) where

import Example.CrossLib
import Test.Syd

-- | This spec lives in @sydtest-mutation-example-gen@ but exercises
-- @Example.CrossLib@ from the separate @sydtest-mutation-example-cross@
-- package.  It is the only coverage for that library, so the cross-package
-- mutation check 'mutation-sydtest-mutation-example-cross' relies on this
-- suite's coverage being attributed across the package boundary.
spec :: Spec
spec = do
  describe "bothPositive" $ do
    it "is True when both are positive" $
      bothPositive 1 1 `shouldBe` True
    it "is False when only the first is positive" $
      bothPositive 1 0 `shouldBe` False
    it "is False when only the second is positive" $
      bothPositive 0 1 `shouldBe` False
    it "is False when neither is positive" $
      bothPositive 0 0 `shouldBe` False
  describe "eitherPositive" $ do
    it "is True when both are positive" $
      eitherPositive 1 1 `shouldBe` True
    it "is True when only the first is positive" $
      eitherPositive 1 0 `shouldBe` True
    it "is True when only the second is positive" $
      eitherPositive 0 1 `shouldBe` True
    it "is False when neither is positive" $
      eitherPositive 0 0 `shouldBe` False
