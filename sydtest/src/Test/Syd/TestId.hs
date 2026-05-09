{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.TestId
  ( TestId (..),
    renderTestId,
    testIdFilterArg,
    parseTestIdFilterArg,
    TestIdTrie (..),
    testIdTrieFromSet,
    testIdTrieFromList,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | An opaque identifier for a single test in a 'Spec'.
--
-- A 'TestId' is stable across runs as long as the spec structure does not
-- change.  When two tests share the same description at the same level, a
-- zero-based per-description sibling index is used to distinguish them; the
-- index is omitted from the human-readable rendering when it is zero.
newtype TestId = TestId [(Text, Int)]
  deriving (Eq, Ord, Show)

-- | Human-readable rendering.  The index suffix @(n)@ is only shown when @n > 0@.
--
-- Example: @"Database > insert > works"@ or @"Database > insert > works (2)"@
renderTestId :: TestId -> Text
renderTestId (TestId steps) = T.intercalate " > " (map renderStep steps)
  where
    renderStep (t, 0) = t
    renderStep (t, n) = t <> " (" <> T.pack (show n) <> ")"

-- | Render a 'TestId' as the argument to @--filter-id@.
--
-- The format is dot-separated steps, where each step is
-- @description@ or @description[n]@ (index omitted when zero).
testIdFilterArg :: TestId -> Text
testIdFilterArg (TestId steps) = T.intercalate "." (map renderStep steps)
  where
    renderStep (t, 0) = t
    renderStep (t, n) = t <> "[" <> T.pack (show n) <> "]"

-- | Parse the output of 'testIdFilterArg' back into a 'TestId'.
-- Returns 'Nothing' if the input is malformed.
parseTestIdFilterArg :: Text -> Maybe TestId
parseTestIdFilterArg t
  | T.null t = Nothing
  | otherwise = TestId <$> mapM parseStep (T.splitOn "." t)
  where
    parseStep s
      | T.null s = Nothing
      | T.last s == ']' =
          let (name, rest) = T.breakOn "[" s
           in case T.stripPrefix "[" rest >>= T.stripSuffix "]" of
                Nothing -> Nothing
                Just idxT -> case reads (T.unpack idxT) of
                  [(n, "")] -> Just (name, n)
                  _ -> Nothing
      | otherwise = Just (s, 0)

-- | A trie over 'TestId' paths, used for efficient filtering of a 'TestForest'.
data TestIdTrie
  = -- | This path identifies a selected leaf test.
    TrieLeaf
  | -- | Intermediate node: descend into matching children.
    TrieNode (Map (Text, Int) TestIdTrie)
  deriving (Eq, Show)

instance Semigroup TestIdTrie where
  TrieLeaf <> _ = TrieLeaf
  _ <> TrieLeaf = TrieLeaf
  TrieNode m1 <> TrieNode m2 = TrieNode (Map.unionWith (<>) m1 m2)

instance Monoid TestIdTrie where
  mempty = TrieNode Map.empty

-- | Build a 'TestIdTrie' from a set of 'TestId's.
testIdTrieFromSet :: Set TestId -> TestIdTrie
testIdTrieFromSet = testIdTrieFromList . Set.toList

-- | Build a 'TestIdTrie' from a list of 'TestId's.
testIdTrieFromList :: [TestId] -> TestIdTrie
testIdTrieFromList = foldr insertId (TrieNode Map.empty)
  where
    insertId (TestId []) _ = TrieLeaf
    insertId (TestId ((t, i) : rest)) trie =
      let child = insertId (TestId rest) (childOf t i trie)
       in case trie of
            TrieLeaf -> TrieLeaf
            TrieNode m -> TrieNode (Map.insert (t, i) child m)

    childOf t i = \case
      TrieLeaf -> TrieLeaf
      TrieNode m -> Map.findWithDefault (TrieNode Map.empty) (t, i) m
