{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Def.Around where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Kind
import Test.QuickCheck.IO ()
import Test.Syd.Def.TestDefM
import Test.Syd.HList
import Test.Syd.Run
import Test.Syd.SpecDef

-- | Run a custom action before every spec item, to set up an inner resource 'inner'.
--
-- Note that this function turns off shrinking.
-- See https://github.com/nick8325/quickcheck/issues/331
before ::
  -- | The function to run before every test, to produce the inner resource
  IO inner ->
  TestDefM outers inner result ->
  TestDefM outers () result
before action = beforeWith $ \() -> action

-- | Run a custom action before every spec item without setting up any inner resources.
--
-- Note that this function turns off shrinking.
-- See https://github.com/nick8325/quickcheck/issues/331
before_ ::
  -- | The function to run before every test
  IO () ->
  TestDefM outers inner result ->
  TestDefM outers inner result
before_ action = beforeWith $ \inner -> do
  action
  pure inner

-- | Run a custom action before every spec item, to set up an inner resource 'newInner' using the previously set up resource 'oldInner'
--
-- Note that this function turns off shrinking.
-- See https://github.com/nick8325/quickcheck/issues/331
beforeWith ::
  forall outers oldInner newInner result.
  (oldInner -> IO newInner) ->
  TestDefM outers newInner result ->
  TestDefM outers oldInner result
beforeWith action = beforeWith' (\(_ :: HList outers) -> action)

-- | Run a custom action before every spec item, to set up an inner resource 'newInner' using the previously set up resource 'oldInner' and potentially any of the outer resources
--
-- Note that this function turns off shrinking.
-- See https://github.com/nick8325/quickcheck/issues/331
beforeWith' ::
  HContains outers outer =>
  (outer -> oldInner -> IO newInner) ->
  TestDefM outers newInner result ->
  TestDefM outers oldInner result
beforeWith' action = aroundWith' $ \func outer inner -> action outer inner >>= func outer

-- | Run a custom action after every spec item, using the inner resource 'c'.
--
-- Note that this function turns off shrinking.
-- See https://github.com/nick8325/quickcheck/issues/331
after ::
  -- | The function to run after every test, using the inner resource
  (inner -> IO ()) ->
  TestDefM outers inner result ->
  TestDefM outers inner result
after action = aroundWith $ \e x -> e x `finally` action x

-- | Run a custom action after every spec item without using any inner resources.
--
-- Note that this function turns off shrinking.
-- See https://github.com/nick8325/quickcheck/issues/331
after_ ::
  -- | The function to run after every test
  IO () ->
  TestDefM outers inner result ->
  TestDefM outers inner result
after_ action = after $ \_ -> action

-- | Run a custom action before and/or after every spec item, to provide access to an inner resource 'c'.
--
-- See the @FOOTGUN@ note in the docs for 'around_'.
--
-- Note that this function turns off shrinking.
-- See https://github.com/nick8325/quickcheck/issues/331
around ::
  -- | The function to provide the inner resource around every test
  ((inner -> IO ()) -> IO ()) ->
  TestDefM outers inner result ->
  TestDefM outers () result
around action = aroundWith $ \e () -> action e

-- | Run a custom action before and/or after every spec item without accessing any inner resources.
--
-- It is important that the wrapper function that you provide runs the action that it gets _exactly once_.
--
-- == __FOOTGUN__
--
-- This combinator gives the programmer a lot of power.
-- In fact, it gives the programmer enough power to break the test framework.
-- Indeed, you can provide a wrapper function that just _doesn't_ run the function like this:
--
-- > spec :: Spec
-- > spec = do
-- >    let don'tDo :: IO () -> IO ()
-- >        don'tDo _ = pure ()
-- >    around_ don'tDo $ do
-- >      it "should pass" True
--
-- During execution, you'll then get an error like this:
--
-- > thread blocked indefinitely in an MVar operation
--
-- The same problem exists when using 'Test.Syd.Def.Around.aroundAll_'.
--
-- The same thing will go wrong if you run the given action more than once like this:
--
-- > spec :: Spec
-- > spec = do
-- >    let doTwice :: IO () -> IO ()
-- >        doTwice f = f >> f
-- >    around_ doTwice $ do
-- >      it "should pass" True
--
--
-- Note: If you're interested in fixing this, talk to me, but only after GHC has gotten impredicative types because that will likely be a requirement.
--
-- Note that this function turns off shrinking.
-- See https://github.com/nick8325/quickcheck/issues/331
around_ ::
  -- | The function to wrap every test with
  (IO () -> IO ()) ->
  TestDefM outers inner result ->
  TestDefM outers inner result
around_ action = aroundWith $ \e a -> action (e a)

-- | Run a custom action before and/or after every spec item, to provide access to an inner resource 'c' while using the inner resource 'd'.
--
-- See the @FOOTGUN@ note in the docs for 'around_'.
--
-- Note that this function turns off shrinking.
-- See https://github.com/nick8325/quickcheck/issues/331
aroundWith ::
  forall newInner oldInner outers result.
  ((newInner -> IO ()) -> (oldInner -> IO ())) ->
  TestDefM outers newInner result ->
  TestDefM outers oldInner result
aroundWith func =
  aroundWith' $
    \(takeAC :: HList outers -> newInner -> IO ()) -- Just to make sure the 'a' is not ambiguous.
     a
     d ->
        func (\c -> takeAC a c) d

-- | Run a custom action around every spec item, to provide access to an inner resource 'newInner' while using the inner resource 'oldInner' and any outer resource available.
--
-- Note that this function turns off shrinking.
-- See https://github.com/nick8325/quickcheck/issues/331
aroundWith' ::
  forall newInner oldInner outer result (outers :: [Type]).
  HContains outers outer =>
  -- | The function that provides the new inner resource using the old resource.
  -- It can also use and modify the outer resource
  ((outer -> newInner -> IO ()) -> (outer -> oldInner -> IO ())) ->
  TestDefM outers newInner result ->
  TestDefM outers oldInner result
aroundWith' func (TestDefM rwst) =
  local (\tde -> tde {testDefEnvTestRunSettings = (testDefEnvTestRunSettings tde) {testRunSettingMaxShrinks = 0}}) $
    TestDefM $
      flip mapWriterT rwst $ \inner -> do
        (res, forest) <- inner
        -- a: outers
        -- c: newInner
        -- d: oldInner
        let modifyVal ::
              forall x.
              HContains x outer =>
              (ProgressReporter -> ((HList x -> newInner -> IO ()) -> IO ()) -> IO TestRunResult) ->
              ProgressReporter ->
              ((HList x -> oldInner -> IO ()) -> IO ()) ->
              IO TestRunResult
            modifyVal takeSupplyXC progressReporter supplyXD =
              let supplyXC :: (HList x -> newInner -> IO ()) -> IO ()
                  supplyXC takeXC =
                    let takeXD :: HList x -> oldInner -> IO ()
                        takeXD x d =
                          let takeAC _ c = takeXC x c
                           in func takeAC (getElem x) d
                     in supplyXD takeXD
               in takeSupplyXC progressReporter supplyXC

            -- For this function to work recursively, the first parameter of the input and the output types must be the same
            modifyTree ::
              forall x extra. HContains x outer => SpecDefTree x newInner extra -> SpecDefTree x oldInner extra
            modifyTree = \case
              DefDescribeNode t sdf -> DefDescribeNode t $ modifyForest sdf
              DefPendingNode t mr -> DefPendingNode t mr
              DefSpecifyNode t td e -> DefSpecifyNode t (modifyVal <$> td) e
              DefWrapNode f sdf -> DefWrapNode f $ modifyForest sdf
              DefBeforeAllNode f sdf -> DefBeforeAllNode f $ modifyForest sdf
              DefAroundAllNode f sdf -> DefAroundAllNode f $ modifyForest sdf
              DefAroundAllWithNode f sdf -> DefAroundAllWithNode f $ modifyForest sdf
              DefAfterAllNode f sdf -> DefAfterAllNode f $ modifyForest sdf
              DefParallelismNode f sdf -> DefParallelismNode f $ modifyForest sdf
              DefRandomisationNode f sdf -> DefRandomisationNode f $ modifyForest sdf
              DefRetriesNode f sdf -> DefRetriesNode f $ modifyForest sdf
              DefFlakinessNode f sdf -> DefFlakinessNode f $ modifyForest sdf
            modifyForest ::
              forall x extra.
              HContains x outer =>
              SpecDefForest x newInner extra ->
              SpecDefForest x oldInner extra
            modifyForest = map modifyTree
        let forest' :: SpecDefForest outers oldInner ()
            forest' = modifyForest forest
        pure (res, forest')
