module Test.Syd.SequentialSpec (spec) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Test.Syd

spec :: Spec
spec = sequential . doNotRandomiseExecutionOrder $ do
  var <- liftIO $ newMVar ()
  let times = 10000
  let codeThatMustBeRunAlone = do
        replicateM_ times $ do
          gotIt <- tryTakeMVar var
          case gotIt of
            Nothing -> expectationFailure "Couldn't get the mvar, this means that the 'sequential' above is broken."
            Just () -> putMVar var ()
  it "does not happen at same time as the other test (A)" codeThatMustBeRunAlone
  it "does not happen at same time as the other test (B)" codeThatMustBeRunAlone
