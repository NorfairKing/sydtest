{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Bar.BarSpec where

import Test.Syd

spec :: TestDef (String : otherOuters) ()
spec = do
  describe "can access outer resource" $
    itWithOuter "bar is bar" $
      \s -> s `shouldBe` "bar"
