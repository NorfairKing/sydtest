module AroundSpec (spec) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Test.Syd

spec :: Spec
spec = do
  describe "before" $ do
    var <- liftIO $ newTVarIO 1
    let readAndIncrement = atomically $ stateTVar var $ \i -> (i, i + 1)
    before_ (() <$ readAndIncrement) $
      before readAndIncrement $ do
        it "reads 2" $ \i ->
          i `shouldBe` 2
        it "reads 4" $ \i ->
          i `shouldBe` 4
        it "reads 6" $ \i ->
          i `shouldBe` 6
