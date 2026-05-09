{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Mutation
  ( -- * Test identifiers
    TestId (..),
    renderTestId,
    parseTestIdFilterArg,

    -- * Test identifier tries
    TestIdTrie (..),
    testIdTrieFromSet,
    testIdTrieFromList,

    -- * Forest operations
    flattenTestForestWithIds,
    filterTestForestByTrie,

    -- * Running
    execTestDefM',
    getTestIds,
    runFilteredForest,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Syd
import Test.Syd.OptParse

-- * TestId

-- | An opaque identifier for a single test in a 'Spec'.
--
-- A 'TestId' is stable across runs as long as the spec structure does not
-- change.  When two tests share the same description at the same level, a
-- zero-based per-description sibling index is used to distinguish them; the
-- index is omitted when it is zero.
newtype TestId = TestId [(Text, Int)]
  deriving (Eq, Ord, Show)

-- | Render a 'TestId' as a human-readable string that is also parseable by
-- 'parseTestIdFilterArg' and usable as the argument to @--filter-id@.
--
-- = Format
--
-- Steps are separated by @.@.  The index suffix @:n@ (for @n > 0@) is
-- appended after the description.  Literal @\@, @.@, and @:@ in
-- descriptions are escaped as @\\@, @\.@, and @\:@.
--
-- Example: @"Database.insert.works"@ or @"Database.insert.works:2"@
--
-- = Shell usage
--
-- The rendered form is designed to be passed as a @--filter-id@ argument
-- on the command line wrapped in __single quotes__, which in both bash and
-- zsh pass every character literally with no interpretation at all:
--
-- > --filter-id 'Database.insert.works:2'
--
-- The only character that cannot appear inside single quotes is @'@ itself.
-- Since neither the separator (@.@) nor the index suffix (@:n@) nor the
-- escape character (@\@) involve @'@, and since @\@ is passed literally
-- inside single quotes, the rendered form never requires any shell escaping
-- beyond the surrounding single quotes — regardless of what characters
-- appear in the original test descriptions.
renderTestId :: TestId -> Text
renderTestId (TestId steps) = T.intercalate "." (map renderStep steps)
  where
    renderStep (t, 0) = escapeDesc t
    renderStep (t, n) = escapeDesc t <> ":" <> T.pack (show n)

escapeDesc :: Text -> Text
escapeDesc = T.concatMap $ \case
  '\\' -> "\\\\"
  '.' -> "\\."
  ':' -> "\\:"
  c -> T.singleton c

unescapeDesc :: Text -> Text
unescapeDesc t = T.pack (go (T.unpack t))
  where
    go [] = []
    go ('\\' : c : rest) = c : go rest
    go (c : rest) = c : go rest

-- | Parse the output of 'renderTestId' back into a 'TestId'.
-- Returns 'Nothing' if the input is malformed or empty.
parseTestIdFilterArg :: Text -> Maybe TestId
parseTestIdFilterArg t
  | T.null t = Nothing
  | otherwise = TestId <$> parseSteps (T.unpack t)

-- | Split on unescaped dots and parse each raw (still-escaped) step.
parseSteps :: String -> Maybe [(Text, Int)]
parseSteps = fmap reverse . go [] []
  where
    -- Collect raw chars of the current step (reversed), splitting on
    -- unescaped '.'.  Escape sequences are kept intact for 'finishStep'.
    go steps acc [] =
      (: steps) <$> finishStep (reverse acc)
    go steps acc ('\\' : c : rest) =
      go steps (c : '\\' : acc) rest
    go _ _ ['\\'] = Nothing
    go steps acc ('.' : rest) = do
      step <- finishStep (reverse acc)
      go (step : steps) [] rest
    go steps acc (c : rest) =
      go steps (c : acc) rest

    -- Parse a single raw (escaped) step into (description, index).
    -- The index suffix is an unescaped ':' followed only by digits at the end.
    -- If the part after the last unescaped ':' is not all digits, the whole
    -- string is treated as a description with index 0.
    finishStep :: String -> Maybe (Text, Int)
    finishStep [] = Nothing
    finishStep s =
      case splitAtLastUnescapedColon s of
        Just (rawName, idxStr)
          | not (null idxStr) ->
              case reads idxStr of
                [(n, "")] -> Just (unescapeDesc (T.pack rawName), n)
                _ -> Just (unescapeDesc (T.pack s), 0)
        _ -> Just (unescapeDesc (T.pack s), 0)

    -- Find the last unescaped ':' and split there.
    splitAtLastUnescapedColon :: String -> Maybe (String, String)
    splitAtLastUnescapedColon s =
      case findUnescapedColons s of
        [] -> Nothing
        is ->
          let i = last is
           in Just (take i s, drop (i + 1) s)

    -- Indices of unescaped ':' characters in the string.
    findUnescapedColons :: String -> [Int]
    findUnescapedColons = go2 0 False
      where
        go2 _ _ [] = []
        go2 i True (_ : rest) = go2 (i + 1) False rest
        go2 i False ('\\' : rest) = go2 (i + 1) True rest
        go2 i False (':' : rest) = i : go2 (i + 1) False rest
        go2 i False (_ : rest) = go2 (i + 1) False rest

-- * TestIdTrie

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

-- * Forest operations

-- | Flatten a 'TestForest' into a list of '(TestId, value)' pairs.
--
-- 'TestId's are assigned by traversing the forest in order.  Sibling nodes
-- with the same description text receive a zero-based per-description index
-- so that duplicate descriptions are still uniquely identified.
flattenTestForestWithIds :: SpecDefForest '[] () result -> [(TestId, result)]
flattenTestForestWithIds = goForest []
  where
    goForest :: [(Text, Int)] -> SpecDefForest outers inner c -> [(TestId, c)]
    goForest path = snd . foldl (goTree path) (Map.empty, [])

    goTree ::
      [(Text, Int)] ->
      (Map Text Int, [(TestId, c)]) ->
      SpecDefTree outers inner c ->
      (Map Text Int, [(TestId, c)])
    goTree path (seen, acc) tree =
      let (mkey, seen') = nextKey seen tree
       in case tree of
            DefSpecifyNode _ _ e ->
              case mkey of
                Nothing -> (seen', acc)
                Just key -> (seen', acc ++ [(TestId (reverse (key : path)), e)])
            DefPendingNode _ _ -> (seen', acc)
            DefDescribeNode _ sub ->
              case mkey of
                Nothing -> (seen', acc)
                Just key -> (seen', acc ++ goForest (key : path) sub)
            DefSetupNode _ sub -> (seen', acc ++ goForest path sub)
            DefBeforeAllNode _ sub -> (seen', acc ++ goForest path sub)
            DefBeforeAllWithNode _ sub -> (seen', acc ++ goForest path sub)
            DefWrapNode _ sub -> (seen', acc ++ goForest path sub)
            DefAroundAllNode _ sub -> (seen', acc ++ goForest path sub)
            DefAroundAllWithNode _ sub -> (seen', acc ++ goForest path sub)
            DefAfterAllNode _ sub -> (seen', acc ++ goForest path sub)
            DefParallelismNode _ sub -> (seen', acc ++ goForest path sub)
            DefRandomisationNode _ sub -> (seen', acc ++ goForest path sub)
            DefTimeoutNode _ sub -> (seen', acc ++ goForest path sub)
            DefRetriesNode _ sub -> (seen', acc ++ goForest path sub)
            DefFlakinessNode _ sub -> (seen', acc ++ goForest path sub)
            DefExpectationNode _ sub -> (seen', acc ++ goForest path sub)

    nextKey :: Map Text Int -> SpecDefTree outers inner c -> (Maybe (Text, Int), Map Text Int)
    nextKey seen tree = case descriptionOf tree of
      Nothing -> (Nothing, seen)
      Just t ->
        let idx = Map.findWithDefault 0 t seen
         in (Just (t, idx), Map.insert t (idx + 1) seen)

    descriptionOf :: SpecDefTree outers inner c -> Maybe Text
    descriptionOf = \case
      DefSpecifyNode t _ _ -> Just t
      DefPendingNode t _ -> Just t
      DefDescribeNode t _ -> Just t
      _ -> Nothing

-- | Filter a 'TestForest' to only the tests present in the given 'TestIdTrie'.
--
-- Wrapper nodes (setup, before-all, around-all, etc.) are kept whenever any
-- of their children are kept.
filterTestForestByTrie :: TestIdTrie -> TestForest '[] () -> TestForest '[] ()
filterTestForestByTrie trie = snd . filterForest trie Map.empty
  where
    filterForest ::
      TestIdTrie ->
      Map Text Int ->
      SpecDefForest outers inner () ->
      (Map Text Int, SpecDefForest outers inner ())
    filterForest t seen = foldl (filterTree t) (seen, [])

    filterTree ::
      TestIdTrie ->
      (Map Text Int, SpecDefForest outers inner ()) ->
      SpecDefTree outers inner () ->
      (Map Text Int, SpecDefForest outers inner ())
    filterTree t (seen, acc) tree =
      let (mkey, seen') = nextKey seen tree
       in case tree of
            DefSpecifyNode name td e ->
              case mkey of
                Nothing -> (seen', acc)
                Just key -> case matchLeaf t key of
                  False -> (seen', acc)
                  True -> (seen', acc ++ [DefSpecifyNode name td e])
            DefPendingNode _ _ -> (seen', acc)
            DefDescribeNode name sub ->
              case mkey of
                Nothing -> (seen', acc)
                Just key -> case stepTrie t key of
                  Nothing -> (seen', acc)
                  Just subTrie ->
                    let (_, sub') = filterForest subTrie Map.empty sub
                     in if null sub' then (seen', acc) else (seen', acc ++ [DefDescribeNode name sub'])
            DefSetupNode func sub -> keepWrapper seen' acc (DefSetupNode func) (filterForest t Map.empty sub)
            DefBeforeAllNode func sub -> keepWrapper seen' acc (DefBeforeAllNode func) (filterForest t Map.empty sub)
            DefBeforeAllWithNode func sub -> keepWrapper seen' acc (DefBeforeAllWithNode func) (filterForest t Map.empty sub)
            DefWrapNode func sub -> keepWrapper seen' acc (DefWrapNode func) (filterForest t Map.empty sub)
            DefAroundAllNode func sub -> keepWrapper seen' acc (DefAroundAllNode func) (filterForest t Map.empty sub)
            DefAroundAllWithNode func sub -> keepWrapper seen' acc (DefAroundAllWithNode func) (filterForest t Map.empty sub)
            DefAfterAllNode func sub -> keepWrapper seen' acc (DefAfterAllNode func) (filterForest t Map.empty sub)
            DefParallelismNode p sub -> keepWrapper seen' acc (DefParallelismNode p) (filterForest t Map.empty sub)
            DefRandomisationNode p sub -> keepWrapper seen' acc (DefRandomisationNode p) (filterForest t Map.empty sub)
            DefTimeoutNode f sub -> keepWrapper seen' acc (DefTimeoutNode f) (filterForest t Map.empty sub)
            DefRetriesNode f sub -> keepWrapper seen' acc (DefRetriesNode f) (filterForest t Map.empty sub)
            DefFlakinessNode fm sub -> keepWrapper seen' acc (DefFlakinessNode fm) (filterForest t Map.empty sub)
            DefExpectationNode em sub -> keepWrapper seen' acc (DefExpectationNode em) (filterForest t Map.empty sub)

    keepWrapper ::
      Map Text Int ->
      SpecDefForest outers inner () ->
      (SpecDefForest outers2 inner2 () -> SpecDefTree outers inner ()) ->
      (Map Text Int, SpecDefForest outers2 inner2 ()) ->
      (Map Text Int, SpecDefForest outers inner ())
    keepWrapper seen' acc wrap (_, sub')
      | null sub' = (seen', acc)
      | otherwise = (seen', acc ++ [wrap sub'])

    matchLeaf :: TestIdTrie -> (Text, Int) -> Bool
    matchLeaf TrieLeaf _ = True
    matchLeaf (TrieNode m) key = case Map.lookup key m of
      Just TrieLeaf -> True
      _ -> False

    stepTrie :: TestIdTrie -> (Text, Int) -> Maybe TestIdTrie
    stepTrie TrieLeaf _ = Just TrieLeaf
    stepTrie (TrieNode m) key = Map.lookup key m

    nextKey :: Map Text Int -> SpecDefTree outers inner c -> (Maybe (Text, Int), Map Text Int)
    nextKey seen tree = case descriptionOf tree of
      Nothing -> (Nothing, seen)
      Just t ->
        let idx = Map.findWithDefault 0 t seen
         in (Just (t, idx), Map.insert t (idx + 1) seen)

    descriptionOf :: SpecDefTree outers inner c -> Maybe Text
    descriptionOf = \case
      DefSpecifyNode t _ _ -> Just t
      DefPendingNode t _ -> Just t
      DefDescribeNode t _ -> Just t
      _ -> Nothing

-- * Running

-- | Like 'execTestDefM', but also returns a 'TestIdTrie' covering all tests
-- in the resulting forest.
-- Use this when you need to enumerate test IDs and run the forest without
-- evaluating the 'Spec' twice.
execTestDefM' :: Settings -> Spec -> IO (TestForest '[] (), TestIdTrie)
execTestDefM' sets spec = do
  forest <- execTestDefM sets spec
  let ids = map fst (flattenTestForestWithIds forest)
  pure (forest, testIdTrieFromList ids)

-- | Enumerate all 'TestId's in a 'Spec' without running any tests.
getTestIds :: Spec -> IO [TestId]
getTestIds spec = do
  forest <- execTestDefM defaultSettings spec
  pure $ map fst (flattenTestForestWithIds forest)

-- | Run a pre-built 'TestForest', filtered to only the tests in the given
-- 'TestIdTrie'.  Returns 'True' if all selected tests passed.
--
-- Use 'execTestDefM'' to build the forest and full trie once, then call this
-- function with a per-mutation trie on each iteration.
runFilteredForest :: Settings -> TestForest '[] () -> TestIdTrie -> IO Bool
runFilteredForest settings forest trie = do
  let filtered = filterTestForestByTrie trie forest
  timedResult <- runSpecForestSynchronously settings filtered
  pure $ not $ anyFailedTests settings (timedValue timedResult)
