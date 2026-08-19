module Test.Syd.ParallelWithSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Test.Syd

-- | How many of the tests below may run at once.
bound :: Word
bound = 2

spec :: Spec
spec = parallelWith bound $ do
  -- How many of these tests are running right now, and the most that ever
  -- were. Every test holds for long enough that the others queue up behind
  -- it, so with the bound broken the high-water mark passes it immediately.
  runningVar <- liftIO $ newMVar (0 :: Word)
  highWaterVar <- liftIO $ newMVar (0 :: Word)
  let codeThatMustNotRunMoreThanBoundAtATime = do
        running <- modifyMVar runningVar $ \running -> pure (succ running, succ running)
        modifyMVar_ highWaterVar $ pure . max running
        threadDelay 100000
        modifyMVar_ runningVar $ pure . pred
        highWater <- readMVar highWaterVar
        -- Only the upper bound is under test. Whether the bound is reached
        -- depends on how many threads the suite was given, which is not
        -- something this can assert without depending on the machine.
        highWater `shouldSatisfy` (<= bound)
  mapM_
    (\i -> it (unwords ["runs at most", show bound, "at a time", show (i :: Word)]) codeThatMustNotRunMoreThanBoundAtATime)
    [1 .. 8]
