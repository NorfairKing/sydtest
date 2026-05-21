{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Diff parsing and diff-scoped mutation selection.
--
-- The diff-scoped runner ('Test.Syd.Mutation.Driver.Diff'-driven @diff@
-- subcommand) takes a unified diff and an already-built 'AugmentedManifest'
-- (the cached which-test-covers-which-mutation map) and works out which
-- subset of mutations to run:
--
-- * Mutations whose recorded source span intersects a changed hunk in a
--   /source/ file are selected directly (their own covering tests will kill
--   them).
-- * Mutations covered by a test whose @it@\/@prop@ call site lies inside a
--   changed hunk in a /test/ file are selected too (the changed test should
--   re-kill what it covers).
--
-- All selection is a pure read of cached data: no compilation and no coverage
-- phase happen here.
module Test.Syd.Mutation.Driver.Diff
  ( -- * Hunks
    DiffHunk (..),
    parseUnifiedDiff,

    -- * Path matching
    pathMatchesHunkFile,

    -- * Test location listings
    renderTestLocationLine,
    parseTestLocationLine,
    parseTestLocationsTsv,

    -- * Selection
    mutationsInHunks,
    testsInHunks,
    mutationsCoveredByTests,
    selectMutations,
  )
where

import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Path
import Test.Syd.Mutation.AugmentedManifest
  ( AugmentedManifest (..),
    AugmentedMutationGroup (..),
    AugmentedMutationRecord (..),
  )
import Test.Syd.Mutation.Runtime (MutationId)
import Test.Syd.Mutation.TestId (TestId, parseTestIdFilterArg, renderTestId)

-- | One hunk of a unified diff, attributed to its new-side file.
--
-- Two line ranges are recorded, both 1-based and inclusive on the /new/ side:
--
-- * 'diffHunkAddedRanges': the maximal runs of /added/ (@+@) lines.  Used for
--   /source/ selection, where we want line precision — only mutations whose
--   span overlaps a line that actually changed are selected.
-- * 'diffHunkSpanStart'\/'diffHunkSpanEnd': the whole new-side span of the
--   hunk (context + added lines).  Used for /test/ selection: editing a
--   test's body changes lines in the same hunk as the test's @it@\/@prop@
--   declaration but not necessarily the declaration line itself, so a test
--   counts as changed when its source line falls anywhere in the hunk.
--
-- A pure-deletion hunk has an empty 'diffHunkAddedRanges' (it adds nothing on
-- the new side) but still carries a span covering its surrounding context.
data DiffHunk = DiffHunk
  { diffHunkFile :: !(Path Rel File),
    diffHunkSpanStart :: !Word,
    diffHunkSpanEnd :: !Word,
    diffHunkAddedRanges :: ![(Word, Word)]
  }
  deriving (Show, Eq, Ord)

-- | Parse a unified diff into one 'DiffHunk' per @\@\@@ hunk, recording both
-- the added-line runs (for line-precise source selection) and the whole
-- new-side span (for test selection).
--
-- Recognises @+++ b\/\<path\>@ headers to attribute subsequent hunks to a
-- file and @\@\@ -a,b +c,d \@\@@ hunk headers for the new-side line numbers.
-- @\/dev\/null@ new-side files (whole-file deletions) are skipped.
parseUnifiedDiff :: Text -> Either String [DiffHunk]
parseUnifiedDiff = go Nothing . T.lines
  where
    go ::
      -- \| Current new-side file (from the most recent @+++@ header).
      Maybe (Path Rel File) ->
      [Text] ->
      Either String [DiffHunk]
    go _ [] = Right []
    go mFile (line : rest)
      | Just path <- T.stripPrefix "+++ " line =
          case parseNewSidePath path of
            Right mp -> go mp rest
            Left err -> Left err
      | "@@" `T.isPrefixOf` line =
          case parseHunkHeader line of
            Left err -> Left err
            Right newStart -> do
              let (body, rest') = span isHunkBodyLine rest
                  hunks = case mFile of
                    Nothing -> []
                    Just file -> [hunkFromBody file newStart body]
              (hunks ++) <$> go mFile rest'
      -- @diff --git@, @index@, @--- a/...@, and any other line are skipped;
      -- only @+++@ and @\@\@@ drive parsing.
      | otherwise = go mFile rest

    -- A hunk body line starts with ' ', '+', '-', or '\' (the
    -- "\ No newline at end of file" marker).  Anything else ends the body.
    isHunkBodyLine :: Text -> Bool
    isHunkBodyLine t = case T.uncons t of
      Just (c, _) -> c `elem` (" +-\\" :: String)
      Nothing -> True -- a blank line is a context line with the leading space stripped

-- | Parse the path out of a @+++ b\/path@ header.  Returns @Right Nothing@
-- for @\/dev\/null@ (a deleted file has no new side) and strips the
-- conventional @b\/@ prefix that @git diff@ emits.  A trailing tab-delimited
-- timestamp (as plain @diff -u@ emits) is dropped.
parseNewSidePath :: Text -> Either String (Maybe (Path Rel File))
parseNewSidePath raw =
  let withoutTimestamp = T.takeWhile (/= '\t') raw
      stripped = stripGitPrefix (T.strip withoutTimestamp)
   in if stripped == "/dev/null"
        then Right Nothing
        else case parseRelFile (T.unpack stripped) of
          Just p -> Right (Just p)
          Nothing -> Left ("diff: unparseable new-side path: " ++ show raw)
  where
    stripGitPrefix t = fromMaybe t (T.stripPrefix "b/" t)

-- | Parse the new-side starting line number out of a @\@\@ -a,b +c,d \@\@@
-- header.  Returns the @c@ (1-based start line of the new-side hunk).
parseHunkHeader :: Text -> Either String Word
parseHunkHeader line =
  case T.words line of
    -- ["@@", "-a,b", "+c,d", "@@", ...]; the new-side token starts with '+'.
    ws -> case filter ("+" `T.isPrefixOf`) ws of
      (plusTok : _) ->
        let numPart = T.takeWhile (/= ',') (T.drop 1 plusTok)
         in case reads (T.unpack numPart) of
              [(n, "")] -> Right n
              _ -> Left ("diff: unparseable hunk header: " ++ show line)
      [] -> Left ("diff: hunk header without new-side range: " ++ show line)

-- | Build one 'DiffHunk' from a hunk body.  Walks the body with a new-side
-- line counter — context lines (' ') and added lines (@+@) advance it,
-- deletion lines (@-@) do not (they don't exist on the new side), and the
-- @\\@ no-newline marker is ignored — recording both the maximal runs of
-- added lines and the whole new-side span @[newStart .. lastNewSideLine]@.
hunkFromBody :: Path Rel File -> Word -> [Text] -> DiffHunk
hunkFromBody file newStart body =
  let (addedRanges, nextLine) = walk Nothing [] newStart body
   in DiffHunk
        { diffHunkFile = file,
          diffHunkSpanStart = newStart,
          -- @nextLine@ is one past the last new-side line consumed; the span
          -- end is therefore @nextLine - 1@.  An empty\/all-deletion body
          -- leaves @nextLine == newStart@, so clamp the end up to
          -- @newStart@ (a single-line span at the hunk start).
          diffHunkSpanEnd = if nextLine > newStart then nextLine - 1 else newStart,
          diffHunkAddedRanges = reverse addedRanges
        }
  where
    -- @cur@ is the new-side line number of the next line to consume. @mRun@
    -- is the current open run of added lines, if any.  Returns the closed
    -- added-line runs (newest first) and the final @cur@.
    walk :: Maybe (Word, Word) -> [(Word, Word)] -> Word -> [Text] -> ([(Word, Word)], Word)
    walk mRun done cur [] = (closeRun mRun done, cur)
    walk mRun done cur (l : ls) = case T.uncons l of
      Just ('+', _) ->
        let run' = case mRun of
              Nothing -> (cur, cur)
              Just (s, _) -> (s, cur)
         in walk (Just run') done (cur + 1) ls
      Just ('-', _) -> walk Nothing (closeRun mRun done) cur ls
      Just ('\\', _) -> walk mRun done cur ls
      -- Context line (leading space) or a stripped blank line.
      _ -> walk Nothing (closeRun mRun done) (cur + 1) ls

    closeRun :: Maybe (Word, Word) -> [(Word, Word)] -> [(Word, Word)]
    closeRun Nothing done = done
    closeRun (Just r) done = r : done

-- | Does a /package-relative/ path (as recorded in a manifest's
-- @source_file@ or printed by @--mutation-coverage-list-locations@) match a
-- /repo-relative/ diff hunk file?
--
-- The manifest and the coverage-listing record paths relative to the package
-- directory (e.g. @src\/Example\/Lib.hs@), while a @git diff@ records paths
-- relative to the repository root (e.g.
-- @sydtest-mutation-example\/src\/Example\/Lib.hs@).  We match when the
-- package-relative path is a path-component suffix of the diff path.
--
-- Component-wise (not raw-string) suffixing prevents @b\/Lib.hs@ from
-- matching @ab\/Lib.hs@.
pathMatchesHunkFile ::
  -- | Package-relative path from the manifest or coverage listing.
  Path Rel File ->
  -- | Repo-relative diff hunk file.
  Path Rel File ->
  Bool
pathMatchesHunkFile pkgRel diffPath =
  let pkgComps = splitComponents pkgRel
      diffComps = splitComponents diffPath
   in pkgComps `isSuffixOf` diffComps
  where
    -- Split a relative file path into its '/'-separated components.
    splitComponents :: Path Rel File -> [String]
    splitComponents = filter (/= "") . splitOn '/' . dropTrailingSep . toFilePath
    dropTrailingSep s = case reverse s of
      ('/' : rest) -> reverse rest
      _ -> s
    splitOn c s = foldr step [[]] s
      where
        step ch acc@(cur : rest)
          | ch == c = [] : acc
          | otherwise = (ch : cur) : rest
        step _ [] = [[]]

-- | Render one test-location listing line: @\<rendered-test-id\>\\t\<file\>:\<line\>@.
-- This is the line format emitted by @--mutation-coverage-list-locations@ and
-- cached one-per-suite by the Nix build.
renderTestLocationLine :: TestId -> (Path Rel File, Word) -> Text
renderTestLocationLine tid (file, line) =
  renderTestId tid
    <> "\t"
    <> T.pack (toFilePath file)
    <> ":"
    <> T.pack (show line)

-- | Parse one test-location line produced by 'renderTestLocationLine'.
--
-- Returns 'Nothing' when the line carries no location (no tab — a test whose
-- call stack was empty, hence unmappable to a source line), or when either
-- field fails to parse.  The @\<file\>:\<line\>@ part is split at its /last/
-- colon so paths containing colons still parse.
parseTestLocationLine :: Text -> Maybe (TestId, (Path Rel File, Word))
parseTestLocationLine line
  | T.null line = Nothing
  | otherwise = case T.breakOn "\t" line of
      (_, "") -> Nothing
      (idText, rest) -> do
        tid <- parseTestIdFilterArg idText
        loc <- parseFileLine (T.drop 1 rest)
        Just (tid, loc)
  where
    parseFileLine t = case T.breakOnEnd ":" t of
      ("", _) -> Nothing
      (fileWithColon, lineText) -> do
        rf <- parseRelFile (T.unpack (T.dropEnd 1 fileWithColon))
        n <- case reads (T.unpack lineText) of
          [(parsed, "")] -> Just parsed
          _ -> Nothing
        Just (rf, n)

-- | Parse a whole test-location listing (one suite's @.tsv@ contents) into a
-- @TestId -> (file, line)@ map.  Lines that 'parseTestLocationLine' rejects
-- are dropped.
parseTestLocationsTsv :: Text -> Map.Map TestId (Path Rel File, Word)
parseTestLocationsTsv = Map.fromList . mapMaybe parseTestLocationLine . T.lines

-- | Select every mutation whose recorded source span intersects a changed
-- hunk in a matching file.
mutationsInHunks :: [DiffHunk] -> AugmentedManifest -> Set MutationId
mutationsInHunks hunks (AugmentedManifest groups) =
  Set.fromList
    [ augmentedMutationRecordId rec
    | AugmentedMutationGroup recs <- groups,
      rec <- recs,
      Just srcFile <- [augmentedMutationRecordSourceFile rec],
      any (hunkHitsRecord srcFile rec) hunks
    ]
  where
    -- Source selection is line-precise: the mutation's span must overlap an
    -- actually-added line range, not merely fall within the hunk's context.
    hunkHitsRecord srcFile rec hunk =
      pathMatchesHunkFile srcFile (diffHunkFile hunk)
        && any
          (rangesIntersect (augmentedMutationRecordLine rec, recEndLine rec))
          (diffHunkAddedRanges hunk)
    -- 'end_line' defaults to 0 in the codec for single-line spans; treat 0
    -- (and any end before start) as a single-line span at 'line'.
    recEndLine rec =
      max (augmentedMutationRecordEndLine rec) (augmentedMutationRecordLine rec)

-- | @rangesIntersect (a, b) (c, d)@ is 'True' when the inclusive ranges
-- @[a..b]@ and @[c..d]@ overlap.  Empty ranges (start > end) overlap nothing.
rangesIntersect :: (Word, Word) -> (Word, Word) -> Bool
rangesIntersect (a, b) (c, d) = a <= b && c <= d && a <= d && c <= b

-- | Select every 'TestId' whose source location lies inside a changed hunk's
-- /whole new-side span/ in a matching file.  The map argument is the per-test
-- @TestId -> (sourceFile, line)@ produced by
-- @--mutation-coverage-list-locations@.
--
-- We use the whole hunk span (not just the added-line ranges) because editing
-- a test's body changes lines in the same hunk as the test's @it@\/@prop@
-- declaration without necessarily touching the declaration line itself; the
-- intent of a test-file change is to re-kill what that test covers.
testsInHunks :: [DiffHunk] -> Map.Map TestId (Path Rel File, Word) -> Set TestId
testsInHunks hunks locations =
  Map.keysSet $
    Map.filterWithKey (\_ (file, line) -> any (hits file line) hunks) locations
  where
    hits file line hunk =
      pathMatchesHunkFile file (diffHunkFile hunk)
        && diffHunkSpanStart hunk <= line
        && line <= diffHunkSpanEnd hunk

-- | Select every mutation that any of the given tests covers, looking the
-- coverage up in the augmented manifest's @covering_tests@ (across all
-- suites).
mutationsCoveredByTests :: Set TestId -> AugmentedManifest -> Set MutationId
mutationsCoveredByTests changedTests (AugmentedManifest groups) =
  Set.fromList
    [ augmentedMutationRecordId rec
    | AugmentedMutationGroup recs <- groups,
      rec <- recs,
      coveredByChanged rec
    ]
  where
    coveredByChanged rec =
      any
        (any (`Set.member` changedTests))
        (Map.elems (augmentedMutationRecordCoveringTests rec))

-- | The full diff-scoped selection: the union of the source-diff selection
-- and the test-diff selection.
--
-- @testLocationsBySuite@ maps each suite name to that suite's
-- @TestId -> (file, line)@ map (from @--mutation-coverage-list-locations@).
-- Test ids are unique within a suite; merging across suites is safe because
-- the manifest keys covering tests by suite and 'mutationsCoveredByTests'
-- checks membership regardless of which suite a test came from.
selectMutations ::
  [DiffHunk] ->
  AugmentedManifest ->
  Map.Map Text (Map.Map TestId (Path Rel File, Word)) ->
  Set MutationId
selectMutations hunks manifest testLocationsBySuite =
  let sourceSelected = mutationsInHunks hunks manifest
      allTestLocations = Map.unions (Map.elems testLocationsBySuite)
      changedTests = testsInHunks hunks allTestLocations
      testSelected = mutationsCoveredByTests changedTests manifest
   in Set.union sourceSelected testSelected
