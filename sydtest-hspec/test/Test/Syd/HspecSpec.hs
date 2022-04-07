module Test.Syd.HspecSpec (spec) where

import Control.Concurrent.STM
import Test.Hspec as Hspec
import Test.Hspec.QuickCheck as Hspec
import qualified Test.Syd as Syd
import qualified Test.Syd.Hspec as Syd

spec :: Syd.Spec
spec = Syd.fromHspec exampleHspecSpec

exampleHspecSpec :: Hspec.Spec
exampleHspecSpec = do
  it "adds 3 and 5 together purely" $ 3 + 5 == (8 :: Int)
  it "adds 3 and 5 together in io" $ 3 + 5 `shouldBe` (8 :: Int)
  prop "works for a property as well" $ \ls -> reverse (reverse ls) `shouldBe` (ls :: [Int])
  describe "before" $ do
    var <- runIO $ newTVarIO (1 :: Int)
    let readAndIncrement = atomically $ stateTVar var $ \i -> (i + 1, i + 1)
    before readAndIncrement $ do
      it "reads 2" $ \i ->
        i `shouldBe` 2
      it "reads 3" $ \i ->
        i `shouldBe` 3
      it "reads 4" $ \i ->
        i `shouldBe` 4
