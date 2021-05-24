{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Scratchpad where

import Control.Concurrent.STM
import Control.Exception
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

data Property i where
  Leaf :: (i -> IO ()) -> Property i
  ForAll :: Show a => Gen a -> (a -> [a]) -> (a -> Property i) -> Property i

class Testable prop i where
  property :: prop -> Property i

instance Testable (Property i) i where
  property = id

instance Testable (IO ()) i where
  property = Leaf . const

instance (Show a, Arbitrary a, Testable prop i) => Testable (a -> prop) i where
  property p = ForAll arbitrary shrink $ \a -> property (p a)

instance {-# OVERLAPPING #-} Testable (i -> IO ()) i where
  property = Leaf

forAllShrink :: (Show a, Testable prop i) => Gen a -> (a -> [a]) -> (a -> prop) -> Property i
forAllShrink gen shrinker func = ForAll gen shrinker (property . func)

forAll :: (Show a, Testable prop i) => Gen a -> (a -> prop) -> Property i
forAll g p = forAllShrink g (const []) p

-- | The results of one example being tested, with all the necessary information to do shrinking still there.
--
-- We'll have to shrink if the bottom bool is False
data SingleResultTree i where
  LeafResult :: (i -> IO ()) -> Bool -> SingleResultTree i
  ForAllResult :: Show a => Seed -> Size -> a -> (a -> [a]) -> (a -> Property i) -> SingleResultTree i -> SingleResultTree i

type Seed = QCGen

type Size = Int

didItCrash :: IO () -> IO Bool
didItCrash action = do
  (action >> pure True)
    `catches` [
                -- Re-throw AsyncException, otherwise execution will not terminate on SIGINT (ctrl-c).
                Handler (\e -> throw (e :: AsyncException)),
                -- Catch all the rest as a string
                Handler (\(e :: SomeException) -> print e >> return False)
              ]

runSingleExample :: forall i. Seed -> Size -> (forall r. (i -> IO r) -> IO r) -> Property i -> IO (SingleResultTree i)
runSingleExample seed size wrapper prop = do
  let go :: Property i -> IO (SingleResultTree i)
      go = \case
        Leaf io -> LeafResult io <$> didItCrash (wrapper io)
        ForAll gen shrinker propFunc -> do
          let generatedValue = unGen gen seed size
          ForAllResult seed size generatedValue shrinker propFunc <$> go (propFunc generatedValue)
  go prop

didFail :: SingleResultTree i -> Bool
didFail = go
  where
    go = \case
      LeafResult _ b -> not b
      ForAllResult _ _ _ _ _ srt -> go srt

-- This could be adapted to return result that contains the number of shrinks, the failing example, etc.
runShrunkExampleBecauseTheTestFailed :: (forall r. (i -> IO r) -> IO r) -> SingleResultTree i -> IO Bool
runShrunkExampleBecauseTheTestFailed wrapper srt_ = do
  putStrLn "Starting to shrink"
  go srt_
  where
    go = \case
      LeafResult io _ -> didItCrash (wrapper io)
      ForAllResult seed size generatedValue shrinker propFunc originalResult -> do
        let go2 [] = do
              print ("return the failure", generatedValue)
              go originalResult
            go2 (smallerVersion : rest) = do
              print ("Trying smaller version", smallerVersion)
              srt <- runSingleExample seed size wrapper (propFunc smallerVersion)
              success <- go srt
              print ("result", success)
              if success
                then -- dindn't fail again, keep looking
                  go2 rest
                else -- Smaller version that still fails, keep going!
                  go $ ForAllResult seed size smallerVersion shrinker propFunc srt
        go2 (shrinker generatedValue)

runPropTest :: (forall r. (i -> IO r) -> IO r) -> Property i -> IO Bool
runPropTest wrapper p = do
  seed <- newQCGen
  putStrLn "Starting the test"
  srt <- runSingleExample seed 30 wrapper p
  if didFail srt
    then runShrunkExampleBecauseTheTestFailed wrapper srt
    else pure True

main :: IO ()
main = do
  var <- newTVarIO True
  let withTrue :: (() -> IO r) -> IO r
      withTrue func = do
        atomically $ writeTVar var True
        r <- func ()
        atomically $ writeTVar var False
        pure r

  b <- runPropTest withTrue $
    forAllShrink (sized $ \n -> pure n) shrink $ \i () -> do
      print i
      b <- readTVarIO var
      if b
        then if i < 3 then pure () else error "test failed"
        else error "Invariant violated." -- this should never happen
  print b
