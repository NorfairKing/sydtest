module Test.Syd.TestUtils where

import Test.Syd.Run
import Test.Syd.SpecDef

eraseTiming :: Timed ResultForest -> Timed ResultForest
eraseTiming = fmap erasedTimedInResultForest . eraseTimed
  where
    eraseTimed :: Timed a -> Timed a
    eraseTimed t =
      t
        { timedBegin = 0,
          timedEnd = 0,
          timedWorker = 0
        }

    erasedTimedInResultForest :: ResultForest -> ResultForest
    erasedTimedInResultForest = fmap (fmap (fmap eraseTimed))
