{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines all the functions you will use to define your test suite.
module Test.Syd.Def
  ( -- * API Functions

    -- ** Declaring tests
    describe,
    it,
    specify,

    -- ** Declaring test dependencies

    -- *** Dependencies around all of a group of tests

    --beforeAll,
    --beforeAll_,
    --afterAll,
    --afterAll_,
    --aroundAll,
    --aroundAll_,
    --aroundAllWith,

    -- *** Dependencies around each of a group of tests
    before,
    before_,
    after,
    after_,
    around,
    around_,
    aroundWith,
    aroundWith',

    -- * Test definition types
    TestDefM (..),
    execTestDefM,
    runTestDefM,

    -- ** Implementation details
    toTestRunSettings,
    filterTestForest,

    -- * Hspec synonyms
    Spec,
    SpecWith,
    SpecM,
  )
where

import Control.Monad.RWS.Strict
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Test.QuickCheck.IO ()
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.SpecDef
import UnliftIO

-- | A synonym for easy migration from hspec
type Spec = SpecWith ()

-- | A synonym for easy migration from hspec
type SpecWith a = SpecM a ()

-- | A synonym for easy migration from hspec
type SpecM a b = TestDefM () a b

-- | The test definition monad
--
-- This type has three parameters:
--
-- * @a@: The type of the result of `aroundAll`
-- * @b@: The type of the result of `around`
-- * @c@: The result
--
-- In practice, all of these three parameters should be '()' at the top level.
newtype TestDefM a b c = TestDefM
  { unTestDefM :: RWST TestRunSettings (TestForest a b) () IO c
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TestRunSettings, MonadWriter (TestForest a b), MonadState ())

execTestDefM :: Settings -> TestDefM a b c -> IO (TestForest a b)
execTestDefM sets = fmap snd . runTestDefM sets

runTestDefM :: Settings -> TestDefM a b c -> IO (c, TestForest a b)
runTestDefM sets defFunc = do
  let func = unTestDefM defFunc
  (a, _, testForest) <- runRWST func (toTestRunSettings sets) () -- TODO allow passing in settings from the command-line
  let testForest' = case settingFilter sets of
        Nothing -> testForest
        Just f -> filterTestForest f testForest
  pure (a, testForest')

toTestRunSettings :: Settings -> TestRunSettings
toTestRunSettings Settings {..} =
  TestRunSettings
    { testRunSettingChildProcessOverride = testRunSettingChildProcessOverride defaultTestRunSettings,
      testRunSettingSeed = settingSeed,
      testRunSettingMaxSuccess = settingMaxSuccess,
      testRunSettingMaxSize = settingMaxSize,
      testRunSettingMaxDiscardRatio = settingMaxDiscard,
      testRunSettingMaxShrinks = settingMaxShrinks
    }

filterTestForest :: Text -> SpecDefForest a b c -> SpecDefForest a b c
filterTestForest f = fromMaybe [] . goForest DList.empty
  where
    goForest :: DList Text -> SpecDefForest a b c -> Maybe (SpecDefForest a b c)
    goForest ts sdf = do
      let sdf' = mapMaybe (goTree ts) sdf
      guard $ not $ null sdf'
      pure sdf'
    goTree :: DList Text -> SpecDefTree a b c -> Maybe (SpecDefTree a b c)
    goTree dl = \case
      DefDescribeNode t sdf -> DefDescribeNode t <$> goForest (DList.snoc dl t) sdf
      DefAroundAllNode func sdf -> DefAroundAllNode func <$> goForest dl sdf
      DefSpecifyNode t td e -> do
        let tl = DList.toList (DList.snoc dl t)
        guard $ f `T.isInfixOf` (T.intercalate "." tl)
        pure $ DefSpecifyNode t td e
      DefParallelismNode func sdf -> DefParallelismNode func <$> goForest dl sdf

-- | Declare a test group
describe :: String -> TestDefM a b c -> TestDefM a b c
describe s func = censor ((: []) . DefDescribeNode (T.pack s)) func

-- | Declare a test
--
-- === Example usage:
--
-- ==== Pure test
--
-- > describe "addition" $
-- >     it "adds 3 to 5 to result in 8" $
-- >         3 + 5 `shouldBe` 8
--
-- ==== Property test
--
-- > describe "sort" $
-- >     it "is idempotent" $
-- >         forAllValid $ \ls ->
-- >             sort (sort ls) `shouldBe` (sort (ls :: [Int]))
it :: (HasCallStack, IsTest test) => String -> test -> TestDefM (Arg1 test) (Arg2 test) ()
it s t = do
  sets <- ask
  let testDef =
        TestDef
          { testDefVal = \supplyArgs -> runTest sets (\func -> supplyArgs func) t,
            testDefCallStack = callStack
          }
  tell [DefSpecifyNode (T.pack s) testDef ()]

-- | A synonym for 'it'
specify :: (HasCallStack, IsTest test) => String -> test -> TestDefM (Arg1 test) (Arg2 test) ()
specify = it

-- | Run a custom action before every spec item.
before :: IO c -> TestDefM a c e -> TestDefM a () e
before action = around (action >>=)

-- | Run a custom action before every spec item.
before_ :: IO () -> TestDefM a c e -> TestDefM a c e
before_ action = around_ (action >>)

-- | Run a custom action after every spec item.
after :: (c -> IO ()) -> TestDefM a c e -> TestDefM a c e
after action = aroundWith $ \e x -> e x `finally` action x

-- | Run a custom action after every spec item.
after_ :: IO () -> TestDefM a c e -> TestDefM a c e
after_ action = after $ \_ -> action

-- | Run a custom action before and/or after every spec item.
around :: ((c -> IO ()) -> IO ()) -> TestDefM a c e -> TestDefM a () e
around action = aroundWith $ \e () -> action e

-- | Run a custom action before and/or after every spec item.
around_ :: (IO () -> IO ()) -> TestDefM a c e -> TestDefM a c e
around_ action = aroundWith $ \e a -> action (e a)

aroundWith :: forall a c d r. ((c -> IO ()) -> (d -> IO ())) -> TestDefM a c r -> TestDefM a d r
aroundWith = undefined

aroundWith' :: forall a c d r u. HContains u a => ((a -> c -> IO ()) -> (a -> d -> IO ())) -> TestDefM u c r -> TestDefM u d r
aroundWith' func (TestDefM rwst) = TestDefM $
  flip mapRWST rwst $ \inner -> do
    (res, s, forest) <- inner
    let modifyVal ::
          forall x.
          HContains x a =>
          (((x -> c -> IO ()) -> IO ()) -> IO TestRunResult) ->
          ((x -> d -> IO ()) -> IO ()) ->
          IO TestRunResult
        modifyVal takeSupplyXC supplyXD =
          let supplyXC :: (x -> c -> IO ()) -> IO ()
              supplyXC takeXC =
                let takeXD :: x -> d -> IO ()
                    takeXD x d =
                      let takeAC _ c = takeXC x c
                       in func takeAC (getElem x) d
                 in supplyXD takeXD
           in takeSupplyXC supplyXC
        -- modifyListVal ::
        --   forall x l.
        --   HContains x a =>
        --   (((x -> c -> IO ()) -> IO ()) -> IO TestRunResult) ->
        --   ((x -> d -> IO ()) -> IO ()) ->
        --   IO TestRunResult
        -- modifyListVal takeSupplyXC supplyXD = undefined

        -- For this function to work recursively, the first parameter of the input and the output types must be the same
        modifyTree :: forall x e. HContains x a => SpecDefTree x c e -> SpecDefTree x d e
        modifyTree = \case
          DefDescribeNode t sdf -> DefDescribeNode t $ modifyForest sdf
          DefSpecifyNode t td e -> DefSpecifyNode t (modifyVal <$> td) e
          DefAroundAllNode f sdf -> DefAroundAllNode f $ modifyForest sdf
          DefParallelismNode f sdf -> DefParallelismNode f $ modifyForest sdf
        -- modifyListTree :: forall x l e. HContains x a => SpecDefTree x c e -> SpecDefTree x d e
        -- modifyListTree = \case
        --   DefDescribeNode t sdf -> DefDescribeNode t $ modifyListForest sdf
        --   DefSpecifyNode t td e -> DefSpecifyNode t (modifyListVal <$> td) e
        --   -- DefAroundAllNode f sdf -> DefAroundAllNode f $ modifyListForest sdf
        --   DefParallelismNode f sdf -> DefParallelismNode f $ modifyListForest sdf
        modifyForest :: forall x e. HContains x a => SpecDefForest x c e -> SpecDefForest x d e
        modifyForest = map modifyTree
    -- modifyListForest :: forall x l e. HContains x a => SpecDefForest x c e -> SpecDefForest x d e
    -- modifyListForest = map modifyListTree
    let forest' :: SpecDefForest u d ()
        forest' = modifyForest forest
    pure (res, s, forest')

-- | Run a custom action before all spec items.
-- beforeAll :: IO a -> TestDefM a b e -> TestDefM () b e
-- beforeAll action = aroundAll (action >>=)
--
-- -- | Run a custom action before all spec items.
-- beforeAll_ :: IO () -> TestDefM a b e -> TestDefM a b e
-- beforeAll_ action = aroundAll_ (action >>)
--
-- -- | Run a custom action after all spec items.
-- afterAll :: (a -> IO ()) -> TestDefM a b e -> TestDefM a b e
-- afterAll action = aroundAllWith $ \e x -> e x `finally` action x
--
-- -- | Run a custom action after all spec items.
-- afterAll_ :: IO () -> TestDefM a b e -> TestDefM a b e
-- afterAll_ action = afterAll $ \_ -> action
--
-- -- | Run a custom action before and/or after all spec items.
-- aroundAll :: ((a -> IO ()) -> IO ()) -> TestDefM a b e -> TestDefM () b e
-- aroundAll action = aroundAllWith $ \e () -> action e
--
-- -- | Run a custom action before and/or after all spec items.
-- aroundAll_ :: (IO () -> IO ()) -> TestDefM a b e -> TestDefM a b e
-- aroundAll_ action = aroundAllWith $ \e a -> action (e a)
--
-- aroundAllWith :: forall a b c r. ((a -> IO ()) -> (b -> IO ())) -> TestDefM a c r -> TestDefM b c r
-- aroundAllWith func (TestDefM rwst) = TestDefM $
--   flip mapRWST rwst $ \inner -> do
--     (res, s, forest) <- inner
--     let forest' = [DefAroundAllNode func forest]
--     pure (res, s, forest')
