module Example.ConstConstructorLibSpec (spec) where

import qualified Data.Map.Strict as Map
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

  describe "taggedOn" $
    it "is on" $
      taggedOn `shouldBe` On

  describe "(?:)" $ do
    it "picks the first direction for an even number" $
      (North ?: East) 0 `shouldBe` North
    it "picks the second direction for an odd number" $
      (North ?: East) 1 `shouldBe` East
    it "picks the first direction of another pair for an even number" $
      (South ?: North) 0 `shouldBe` South
    it "picks the second direction of another pair for an odd number" $
      (South ?: North) 1 `shouldBe` North

  describe "pick" $ do
    it "picks north for an even number" $
      pick 0 `shouldBe` North
    it "picks east for an odd number" $
      pick 1 `shouldBe` East

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

  describe "noDirections" $
    it "holds no directions" $
      noDirections `shouldBe` Map.empty

  describe "northAt" $
    it "holds north at the given key" $
      northAt 3 `shouldBe` Map.singleton 3 North

  describe "constantDirection" $
    it "is north" $
      constantDirection () `shouldBe` North

  describe "homeDirection" $
    it "is north" $
      homeDirection `shouldBe` North
