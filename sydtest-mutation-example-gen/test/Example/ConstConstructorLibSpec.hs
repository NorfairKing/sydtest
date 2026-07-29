module Example.ConstConstructorLibSpec (spec) where

import Example.ConstConstructorLib
import Test.Syd

spec :: Spec
spec = do
  describe "directionOf" $ do
    it "sends even numbers north" $
      directionOf 2 `shouldBe` North
    it "sends odd numbers east" $
      directionOf 3 `shouldBe` East

  describe "directionsOf" $
    it "sends each number its own way" $
      directionsOf [2, 3] `shouldBe` [North, East]

  describe "isNorth" $ do
    it "says north is north" $
      isNorth North `shouldBe` True
    it "says east is not north" $
      isNorth East `shouldBe` False
    it "says south is not north" $
      isNorth South `shouldBe` False

  describe "taggedOff" $
    it "is off" $
      taggedOff `shouldBe` Off

  describe "myHead" $ do
    it "has no head for an empty list" $
      myHead ([] :: [Int]) `shouldBe` MyNothing
    it "has the first element as its head" $
      myHead [1 :: Int, 2] `shouldBe` MyJust 1

  describe "unitSquare" $
    it "is the square of side one" $
      unitSquare `shouldBe` Square 1

  describe "ignoreDirection" $
    it "throws the direction away" $
      ignoreDirection South `shouldBe` ()
