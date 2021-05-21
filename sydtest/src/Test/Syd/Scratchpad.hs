{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Scratchpad where

import Control.Concurrent.STM
import Control.Exception
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

data Property i where
  Leaf :: i -> Property i
  WithResource :: (forall r. (a -> IO r) -> IO r) -> (a -> Property i) -> Property i
  ForAll :: Show a => Gen a -> (a -> [a]) -> (a -> Property i) -> Property i

class Testable prop where
  property :: prop -> Property (IO ())

instance Testable (Property (IO ())) where
  property = id

instance Testable (IO ()) where
  property = Leaf

instance (Show a, Arbitrary a, Testable prop) => Testable (a -> prop) where
  property p = ForAll arbitrary shrink $ \a -> property (p a)

forAllShrink :: (Show a, Testable prop) => Gen a -> (a -> [a]) -> (a -> prop) -> Property (IO ())
forAllShrink gen shrinker func = ForAll gen shrinker (property . func)

forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property (IO ())
forAll g p = forAllShrink g (const []) p

wrapProperty :: Testable prop => (forall r. (a -> IO r) -> IO r) -> (a -> prop) -> Property (IO ())
wrapProperty wrapper p = WithResource wrapper (\a -> property (p a))

-- | The results of one example being tested, with all the necessary information to do shrinking still there.
--
-- We'll have to shrink if the bottom bool is False
data SingleResultTree i where
  LeafResult :: Bool -> SingleResultTree i
  WrapperResult :: Seed -> Size -> (forall r. (a -> IO r) -> IO r) -> (a -> Property i) -> SingleResultTree i -> SingleResultTree i
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

runSingleExample :: Testable prop => Seed -> Size -> prop -> IO (SingleResultTree (IO ()))
runSingleExample seed size prop = do
  let go = \case
        Leaf io -> LeafResult <$> didItCrash io
        WithResource wrapper propFunc ->
          wrapper $ \a -> WrapperResult seed size wrapper propFunc <$> go (propFunc a)
        ForAll gen shrinker propFunc -> do
          let generatedValue = unGen gen seed size
          ForAllResult seed size generatedValue shrinker propFunc <$> go (propFunc generatedValue)

  go (property prop)

didFail :: SingleResultTree i -> Bool
didFail = go
  where
    go = \case
      LeafResult b -> not b
      WrapperResult _ _ _ _ srt -> go srt
      ForAllResult _ _ _ _ _ srt -> go srt

-- This could be adapted to return result that contains the number of shrinks, the failing example, etc.
runShrunkExampleBecauseTheTestFailed :: SingleResultTree (IO ()) -> IO Bool
runShrunkExampleBecauseTheTestFailed srt_ = do
  putStrLn "Starting to shrink"
  go srt_
  where
    go = \case
      LeafResult r -> pure r
      WrapperResult seed size wrapper propFunc _originalResult -> do
        -- HERE BE THE DRAGONS
        --
        -- What the hell do we do here?
        -- If we don't rerun the wrapper then the invariants of the wrappers are not maintained
        -- If we do rerun the wrapper then all of the shrinking below is thrown away.
        putStrLn "Running the wrapper while shrinking"
        wrapper $ \a -> do
          srt <- runSingleExample seed size (propFunc a)
          go srt
      ForAllResult seed size generatedValue shrinker propFunc originalResult -> do
        let go2 [] = do
              print ("return the failure", generatedValue)
              go originalResult
            go2 (smallerVersion : rest) = do
              print ("Trying smaller version", smallerVersion)
              srt <- runSingleExample seed size (propFunc smallerVersion)
              success <- go srt
              print ("result", success)
              if success
                then -- dindn't fail again, keep looking
                  go2 rest
                else -- Smaller version that still fails, keep going!
                  go $ ForAllResult seed size smallerVersion shrinker propFunc srt
        go2 (shrinker generatedValue)

runPropTest :: Testable prop => prop -> IO Bool
runPropTest p = do
  seed <- newQCGen
  putStrLn "Starting the test"
  srt <- runSingleExample seed 30 p
  if didFail srt
    then runShrunkExampleBecauseTheTestFailed srt
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

  b <- runPropTest $
    wrapProperty withTrue $ \() -> forAllShrink (sized $ \n -> pure n) shrink $ \i -> do
      print i
      b <- readTVarIO var
      if b
        then if (i < 3) then pure () else error "test failed"
        else error "Invariant violated." -- this should never happen
  print b
