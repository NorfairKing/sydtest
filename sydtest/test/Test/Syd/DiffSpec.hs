{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Syd.DiffSpec (spec) where

import Control.Monad
import Data.String
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Syd
import Test.Syd.Diff

-- Just for this test
instance IsString (Vector Char) where
  fromString = V.fromList

spec :: Spec
spec = do
  describe "getEditScript" $ do
    let exampleSpec :: Vector Char -> Vector Char -> Vector Edit -> Spec
        exampleSpec old new expected =
          it
            ( unwords
                [ "works for",
                  show old,
                  show new,
                  show expected
                ]
            )
            $ do
              let actual = getEditScript old new
              when (actual /= expected) $
                expectationFailure $
                  unlines
                    [ unwords ["actual:", show actual],
                      unwords ["expected:", show expected]
                    ]

    exampleSpec "" "" []
    exampleSpec "a" "a" []
    exampleSpec "a" "b" [Delete 0 1, Insert 1 0 1]
    exampleSpec "ab" "ac" [Delete 1 1, Insert 2 1 1]
    exampleSpec "b" "aba" [Insert 0 0 1, Insert 1 2 1]
    exampleSpec "abc" "acb" [Delete 1 1, Insert 3 2 1]
    exampleSpec "aaa" "aba" [Delete 0 1, Insert 2 1 1]
    exampleSpec "foofoo" "foo" [Delete 1 1, Delete 3 1, Delete 5 1]
    exampleSpec "foo" "foofoo" [Insert 1 1 1, Insert 2 3 1, Insert 3 5 1]

  describe "computeDiffFromEditScript" $ do
    let exampleSpec :: Vector Char -> Vector Char -> Vector Edit -> Vector (Diff Char) -> Spec
        exampleSpec old new editScript expected =
          it
            ( unwords
                [ "works for",
                  show old,
                  show new,
                  show editScript,
                  show expected
                ]
            )
            $ do
              let actual = computeDiffFromEditScript old new editScript
              when (actual /= expected) $
                expectationFailure $
                  unlines
                    [ unwords ["actual:", show actual],
                      unwords ["expected:", show expected]
                    ]

    exampleSpec "" "" [] []
    exampleSpec "a" "a" [] [Both 'a' 'a']
    exampleSpec "a" "b" [Delete 0 1, Insert 1 0 1] [First 'a', Second 'b']
    exampleSpec "ab" "ac" [Delete 1 1, Insert 2 1 1] [Both 'a' 'a', First 'b', Second 'c']
    exampleSpec "b" "aba" [Insert 0 0 1, Insert 1 2 1] [Second 'a', Both 'b' 'b', Second 'a']
    exampleSpec "abc" "acb" [Delete 1 1, Insert 3 2 1] [Both 'a' 'a', First 'b', Both 'c' 'c', Second 'b']
    exampleSpec "aaa" "aba" [Delete 0 1, Insert 2 1 1] [First 'a', Both 'a' 'a', Second 'b', Both 'a' 'a']
    exampleSpec "foofoo" "foo" [Delete 1 1, Delete 3 1, Delete 5 1] [Both 'f' 'f', First 'o', Both 'o' 'o', First 'f', Both 'o' 'o', First 'o']
    exampleSpec "foo" "foofoo" [Insert 1 1 1, Insert 2 3 1, Insert 3 5 1] [Both 'f' 'f', Second 'o', Both 'o' 'o', Second 'f', Both 'o' 'o', Second 'o']

  describe "getVectorDiff" $ do
    let exampleSpec :: Vector Char -> Vector Char -> Vector (Diff Char) -> Spec
        exampleSpec old new expected =
          it
            ( unwords
                [ "works for",
                  show old,
                  show new,
                  show expected
                ]
            )
            $ do
              let actual = getVectorDiff old new
              when (actual /= expected) $
                expectationFailure $
                  unlines
                    [ unwords ["actual:", show actual],
                      unwords ["expected:", show expected]
                    ]
    exampleSpec "" "" []
    exampleSpec "a" "a" [Both 'a' 'a']
    exampleSpec "a" "b" [First 'a', Second 'b']
    exampleSpec "ab" "ac" [Both 'a' 'a', First 'b', Second 'c']
    exampleSpec "aaa" "aba" [First 'a', Both 'a' 'a', Second 'b', Both 'a' 'a']
    exampleSpec "abgdef" "gh" [First 'a', First 'b', Both 'g' 'g', First 'd', First 'e', Second 'h', First 'f']
    exampleSpec "foofoo" "foo" [Both 'f' 'f', First 'o', Both 'o' 'o', First 'f', Both 'o' 'o', First 'o']

    prop "says that any list is entirely different from the empty list (left)" $ \ls ->
      let v = V.fromList (ls :: String)
       in getVectorDiff v "" `shouldBe` V.map First v

    prop "says that any list is entirely different from the empty list (right)" $ \ls ->
      let v = V.fromList (ls :: String)
       in getVectorDiff "" v `shouldBe` V.map Second v

    prop "does not find diffs in identical strings" $ \ls ->
      let v = V.fromList (ls :: String)
       in getVectorDiff v v `shouldBe` V.map (\a -> Both a a) v

    prop "only puts equal characters in 'Both'" $ \(ls1, ls2) ->
      let v1 = V.fromList (ls1 :: String)
          v2 = V.fromList (ls2 :: String)
          diff = getVectorDiff v1 v2
          valid = \case
            First _ -> True
            Second _ -> True
            Both a b -> a == b
       in all valid diff

    let rebuildFirst :: Vector (PolyDiff a b) -> Vector a
        rebuildFirst = V.mapMaybe $ \case
          First a -> Just a
          Second _ -> Nothing
          Both a _ -> Just a
    prop "lets you rebuild the old vector" $ \(ls1, ls2) -> do
      let v1 = V.fromList (ls1 :: String)
          v2 = V.fromList (ls2 :: String)
      rebuildFirst (getVectorDiff v1 v2) `shouldBe` v1

    let rebuildSecond :: Vector (PolyDiff a b) -> Vector b
        rebuildSecond = V.mapMaybe $ \case
          First _ -> Nothing
          Second b -> Just b
          Both _ b -> Just b
    prop "lets you rebuild the new vector" $ \(ls1, ls2) -> do
      let v1 = V.fromList (ls1 :: String)
          v2 = V.fromList (ls2 :: String)
      rebuildSecond (getVectorDiff v1 v2) `shouldBe` v2

    let unrollGroupedDiff :: Vector (PolyDiff (Vector a) (Vector b)) -> Vector (PolyDiff a b)
        unrollGroupedDiff = foldMap unrollPolyDiff

        -- Not performant, only for testing
        unrollPolyDiff :: PolyDiff (Vector a) (Vector b) -> Vector (PolyDiff a b)
        unrollPolyDiff = \case
          First va -> V.map First va
          Second vb -> V.map Second vb
          Both va vb -> V.zipWith Both va vb

    prop "is the same thing as getGroupedVectorDiff, but unrolled (unroll grouped)" $ \(ls1, ls2) ->
      let v1 = V.fromList (ls1 :: String)
          v2 = V.fromList (ls2 :: String)
          grouped = getGroupedVectorDiff v1 v2
          individual = getVectorDiff v1 v2
       in unrollGroupedDiff grouped `shouldBe` individual

  describe "getGroupedVectorDiff" $ do
    prop "says that any list is entirely different from the empty list (left)" $ \ls ->
      let v = V.fromList (ls :: String)
       in getGroupedVectorDiff v "" `shouldBe` (if V.null v then V.empty else V.singleton (First v))

    prop "says that any list is entirely different from the empty list (right)" $ \ls ->
      let v = V.fromList (ls :: String)
       in getGroupedVectorDiff "" v `shouldBe` (if V.null v then V.empty else V.singleton (Second v))

    prop "does not find diffs in identical strings" $ \ls ->
      let v = V.fromList (ls :: String)
       in getGroupedVectorDiff v v `shouldBe` (if V.null v then V.empty else V.singleton (Both v v))

    prop "only puts equal vectors in 'Both'" $ \(ls1, ls2) ->
      let v1 = V.fromList (ls1 :: String)
          v2 = V.fromList (ls2 :: String)
          diff = getGroupedVectorDiff v1 v2
          valid = \case
            First _ -> True
            Second _ -> True
            Both a b -> a == b
       in all valid diff

    let rebuildFirst :: Vector (PolyDiff (Vector a) (Vector b)) -> Vector a
        rebuildFirst =
          V.concat . V.toList
            . V.mapMaybe
              ( \case
                  First a -> Just a
                  Second _ -> Nothing
                  Both a _ -> Just a
              )
    prop "lets you rebuild the old vector" $ \(ls1, ls2) -> do
      let v1 = V.fromList (ls1 :: String)
          v2 = V.fromList (ls2 :: String)
      rebuildFirst (getGroupedVectorDiff v1 v2) `shouldBe` v1

    let rebuildSecond :: Vector (PolyDiff (Vector a) (Vector b)) -> Vector b
        rebuildSecond =
          V.concat . V.toList
            . V.mapMaybe
              ( \case
                  First _ -> Nothing
                  Second b -> Just b
                  Both _ b -> Just b
              )
    prop "lets you rebuild the new vector" $ \(ls1, ls2) -> do
      let v1 = V.fromList (ls1 :: String)
          v2 = V.fromList (ls2 :: String)
      rebuildSecond (getGroupedVectorDiff v1 v2) `shouldBe` v2

    -- Not performant, only for testing.
    let rollupUngroupedDiff :: Vector (PolyDiff a b) -> Vector (PolyDiff (Vector a) (Vector b))
        rollupUngroupedDiff = V.fromList . go . V.toList
          where
            go = \case
              [] -> []
              (d : ds) -> case d of
                First _ ->
                  let (fs, rest) = goFirsts (d : ds)
                   in First (V.fromList fs) : go rest
                Second _ ->
                  let (ss, rest) = goSeconds (d : ds)
                   in Second (V.fromList ss) : go rest
                Both _ _ ->
                  let (bs, rest) = goBoths (d : ds)
                   in Both (V.fromList (map fst bs)) (V.fromList (map snd bs)) : go rest
            goFirsts = \case
              (First a : ds) ->
                let (fs, rest) = goFirsts ds
                 in (a : fs, rest)
              rest -> ([], rest)
            goSeconds = \case
              (Second a : ds) ->
                let (fs, rest) = goSeconds ds
                 in (a : fs, rest)
              rest -> ([], rest)
            goBoths = \case
              (Both a b : ds) ->
                let (fs, rest) = goBoths ds
                 in ((a, b) : fs, rest)
              rest -> ([], rest)

    let squishGroupedDiff :: Vector (PolyDiff (Vector a) (Vector b)) -> Vector (PolyDiff (Vector a) (Vector b))
        squishGroupedDiff = V.fromList . go . V.toList
          where
            go = \case
              [] -> []
              [pd] -> [pd]
              (First a : First b : rest) -> go (First (a <> b) : rest)
              (Second a : Second b : rest) -> go (Second (a <> b) : rest)
              (Both a b : Both c d : rest) -> go (Both (a <> c) (b <> d) : rest)
              (d : rest) -> d : go rest
    prop "is the same thing as getVectorDiff, but rolled up and squished (rollup individual)" $ \(ls1, ls2) ->
      let v1 = V.fromList (ls1 :: String)
          v2 = V.fromList (ls2 :: String)
          grouped = getGroupedVectorDiff v1 v2
          individual = getVectorDiff v1 v2
       in squishGroupedDiff grouped `shouldBe` rollupUngroupedDiff individual

  describe "getStringDiff" $ do
    it "can output a large diff quickly enough if there is no diff" $
      let s = replicate 10000 'a'
       in length (getStringDiff s s) `shouldBe` 10000

    it "can output a large diff quickly enough if it's only diff" $
      length (getStringDiff (replicate 10000 'a') "b") `shouldBe` 10001

  describe "getTextDiff" $ do
    it "can output a large diff quickly enough if there is no diff" $
      let s = T.pack $ replicate 10000 'a'
       in length (getTextDiff s s) `shouldBe` 1

    it "can output a large diff quickly enough if it's only diff" $
      length (getTextDiff (T.pack (replicate 10000 'a')) "b") `shouldBe` 15

  describe "outputEqualityAssertionFailed" $ do
    it "can output a large diff quickly enough" $
      length (outputEqualityAssertionFailed (replicate 10000 'a') "b") `shouldBe` 3
