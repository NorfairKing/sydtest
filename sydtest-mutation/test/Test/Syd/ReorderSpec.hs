{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.ReorderSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Syd
import Test.Syd.Mutation
import Test.Syd.OptParse (Settings (..), defaultSettings)

spec :: Spec
spec = describe "reorderTestForestByTiming" $ do
  it "orders sibling leaves ascending by baseline" $ do
    forest <-
      execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $ do
        it "a" (pure () :: IO ())
        it "b" (pure () :: IO ())
        it "c" (pure () :: IO ())
    let costs =
          Map.fromList
            [ (TestId (("a", 0) :| []), 30),
              (TestId (("b", 0) :| []), 10),
              (TestId (("c", 0) :| []), 20)
            ]
    map (renderTestId . fst) (flattenTestForestWithIds (reorderTestForestByTiming costs forest))
      `shouldBe` ["b", "c", "a"]

  it "keeps equal-cost and untimed siblings in source order (stable, missing = first)" $ do
    forest <-
      execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $ do
        it "timed-late" (pure () :: IO ())
        it "untimed" (pure () :: IO ())
        it "tie1" (pure () :: IO ())
        it "tie2" (pure () :: IO ())
    let costs =
          Map.fromList
            [ (TestId (("timed-late", 0) :| []), 5),
              (TestId (("tie1", 0) :| []), 5),
              (TestId (("tie2", 0) :| []), 5)
            ]
    map (renderTestId . fst) (flattenTestForestWithIds (reorderTestForestByTiming costs forest))
      `shouldBe` ["untimed", "timed-late", "tie1", "tie2"]

  it "reorders a sequential (parallelism) subtree" $ do
    forest <-
      execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $
        sequential $ do
          it "a" (pure () :: IO ())
          it "b" (pure () :: IO ())
          it "c" (pure () :: IO ())
    let costs =
          Map.fromList
            [ (TestId (("a", 0) :| []), 30),
              (TestId (("b", 0) :| []), 10),
              (TestId (("c", 0) :| []), 20)
            ]
    map (renderTestId . fst) (flattenTestForestWithIds (reorderTestForestByTiming costs forest))
      `shouldBe` ["b", "c", "a"]

  it "leaves a doNotRandomiseExecutionOrder subtree in source order" $ do
    forest <-
      execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $
        doNotRandomiseExecutionOrder $ do
          it "z" (pure () :: IO ())
          it "y" (pure () :: IO ())
          it "x" (pure () :: IO ())
    let costs =
          Map.fromList
            [ (TestId (("z", 0) :| []), 3),
              (TestId (("y", 0) :| []), 1),
              (TestId (("x", 0) :| []), 2)
            ]
    map (renderTestId . fst) (flattenTestForestWithIds (reorderTestForestByTiming costs forest))
      `shouldBe` ["z", "y", "x"]

  it "does not descend into a doNotRandomiseExecutionOrder subtree" $ do
    forest <-
      execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $
        doNotRandomiseExecutionOrder $ do
          it "z" (pure () :: IO ())
          randomiseExecutionOrder $ do
            it "n1" (pure () :: IO ())
            it "n2" (pure () :: IO ())
    let costs =
          Map.fromList
            [ (TestId (("z", 0) :| []), 30),
              (TestId (("n1", 0) :| []), 20),
              (TestId (("n2", 0) :| []), 10)
            ]
    map (renderTestId . fst) (flattenTestForestWithIds (reorderTestForestByTiming costs forest))
      `shouldBe` ["z", "n1", "n2"]

  it "reorders a beforeAll_ group as a unit without splitting its setup" $ do
    forest <-
      execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $ do
        it "outside-mid" (pure () :: IO ())
        beforeAll_ (pure ()) $ do
          it "g-cheap" (pure () :: IO ())
          it "g-expensive" (pure () :: IO ())
    let costs =
          Map.fromList
            [ (TestId (("g-cheap", 0) :| []), 1),
              (TestId (("g-expensive", 0) :| []), 100),
              (TestId (("outside-mid", 0) :| []), 50)
            ]
    -- Source order is [outside-mid, g-cheap, g-expensive].  A leaf-global sort
    -- would interleave "outside-mid" (50) between the group's leaves (1, 100).
    -- Keeping the group as a unit puts it first (min cost 1) and leaves
    -- "outside-mid" after both of its leaves.
    map (renderTestId . fst) (flattenTestForestWithIds (reorderTestForestByTiming costs forest))
      `shouldBe` ["g-cheap", "g-expensive", "outside-mid"]

  it "preserves the set of test ids" $ do
    forest <-
      execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $ do
        describe "g1" $ do
          it "a" (pure () :: IO ())
          it "b" (pure () :: IO ())
        it "c" (pure () :: IO ())
    let costs = Map.fromList [(TestId (("g1", 0) :| [("a", 0)]), 5)]
    Set.fromList (map fst (flattenTestForestWithIds (reorderTestForestByTiming costs forest)))
      `shouldBe` Set.fromList (map fst (flattenTestForestWithIds forest))

  describe "reorderForMutationChild (gate)" $ do
    it "reorders when randomisation is enabled and a baseline is present" $ do
      forest <-
        execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $ do
          it "a" (pure () :: IO ())
          it "b" (pure () :: IO ())
          it "c" (pure () :: IO ())
      let costs =
            Map.fromList
              [ (TestId (("a", 0) :| []), 30),
                (TestId (("b", 0) :| []), 10),
                (TestId (("c", 0) :| []), 20)
              ]
      map (renderTestId . fst) (flattenTestForestWithIds (reorderForMutationChild True (Just costs) forest))
        `shouldBe` ["b", "c", "a"]

    it "leaves the forest untouched when randomisation is disabled" $ do
      forest <-
        execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $ do
          it "a" (pure () :: IO ())
          it "b" (pure () :: IO ())
          it "c" (pure () :: IO ())
      let costs =
            Map.fromList
              [ (TestId (("a", 0) :| []), 30),
                (TestId (("b", 0) :| []), 10),
                (TestId (("c", 0) :| []), 20)
              ]
      map (renderTestId . fst) (flattenTestForestWithIds (reorderForMutationChild False (Just costs) forest))
        `shouldBe` ["a", "b", "c"]

    it "leaves the forest untouched when no baseline is available" $ do
      forest <-
        execTestDefM (defaultSettings {settingRandomiseExecutionOrder = False}) $ do
          it "a" (pure () :: IO ())
          it "b" (pure () :: IO ())
          it "c" (pure () :: IO ())
      map (renderTestId . fst) (flattenTestForestWithIds (reorderForMutationChild True Nothing forest))
        `shouldBe` ["a", "b", "c"]
