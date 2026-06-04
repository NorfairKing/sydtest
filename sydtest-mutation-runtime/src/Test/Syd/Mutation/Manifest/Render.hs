{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Colored text rendering for 'MutationRecord' / 'MutationManifest'.
--
-- Shared between the plugin's manifest-emission step (which writes a
-- @.txt@ next to each @.json@) and the test runner's surviving-mutation
-- report.  See 'renderMutationRecord' for the unit of rendering.
module Test.Syd.Mutation.Manifest.Render
  ( renderMutationRecord,
    renderManifest,
    writeManifestTxtFile,
    renderUnifiedDiff,

    -- * Colour helpers (also used by 'sydtest')
    delColour,
    addColour,
    emphasiseIntraLine,
    renderDelSide,
    renderAddSide,
  )
where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Myers.Diff (PolyDiff (..), getGroupedDiff, getTextDiff)
import Path
import Path.IO (ensureDir)
import qualified System.IO as IO
import Test.Syd.Mutation.Manifest (MutationGroup (..), MutationManifest (..), MutationRecord (..))
import Test.Syd.Mutation.Runtime (MutationId (..))
import Text.Colour

-- | Render a single 'MutationRecord' as a header line followed by either:
--
--   * a unified diff body, when the record carries 'mutRecSourceLines' /
--     'mutRecMutatedLines' / context, or
--   * a one-line @- original@ / @+ replacement@ fallback when source lines
--     are not available (e.g. mutations whose source span was
--     'UnhelpfulSpan' and so the plugin couldn't extract any line text).
--
-- The header is @<operator> at <file>:<line>:<colStart>-<colEnd>[ #variant]@,
-- matching the existing surviving-mutation report so reviewers see a
-- single consistent format whether they look at the runtime report or the
-- per-module manifest @.txt@.
renderMutationRecord :: MutationRecord -> [[Chunk]]
renderMutationRecord
  MutationRecord
    { mutRecId,
      mutRecOperator,
      mutRecOriginal,
      mutRecReplacement,
      mutRecModule,
      mutRecLine,
      mutRecColStart,
      mutRecColEnd,
      mutRecSourceFile,
      mutRecSourceLines,
      mutRecMutatedLines,
      mutRecContextBefore,
      mutRecContextAfter
    } =
    let MutationId parts = mutRecId
        filePath = case mutRecSourceFile of
          Just p -> fromRelFile p
          Nothing -> moduleToFilePath (T.unpack mutRecModule)
        variantSuffix = case parts of
          [_, _, _, _, _, _, altIdx] -> " #" ++ altIdx
          _ -> ""
        headerText =
          T.pack $
            T.unpack mutRecOperator
              ++ " at "
              ++ filePath
              ++ ":"
              ++ show mutRecLine
              ++ ":"
              ++ show mutRecColStart
              ++ "-"
              ++ show mutRecColEnd
              ++ variantSuffix
        headerLine = [chunk headerText]
     in case mutRecSourceLines of
          [] ->
            [ headerLine,
              [fore red (chunk ("    - " <> mutRecOriginal))],
              [fore green (chunk ("    + " <> mutRecReplacement))]
            ]
          _ ->
            headerLine
              : renderUnifiedDiff
                (fromIntegral mutRecLine)
                mutRecContextBefore
                mutRecSourceLines
                mutRecMutatedLines
                mutRecContextAfter
    where
      moduleToFilePath m = map (\c -> if c == '.' then '/' else c) m ++ ".hs"

-- | Render every record in a manifest, one per group with a blank line in
-- between.  The leading argument is the module name, used for the
-- top-of-file header.
--
-- The header is followed by a count line ("@N mutations in M groups@") so
-- an empty manifest doesn't look like a rendering accident, and so
-- reviewers can sanity-check the totals at a glance.
--
-- Records appear in manifest order (which mirrors plugin discovery order);
-- the renderer makes no attempt to re-sort.
renderManifest :: String -> MutationManifest -> [[Chunk]]
renderManifest moduleName (MutationManifest groups) =
  let records = concatMap (\(MutationGroup rs) -> rs) groups
      nRecords = length records
      nGroups = length groups
      header = [fore cyan (chunk (T.pack ("# " ++ moduleName)))]
      countLine =
        [ chunk (T.pack (show nRecords ++ " " ++ pluralise nRecords "mutation" ++ " in " ++ show nGroups ++ " " ++ pluralise nGroups "group"))
        ]
      renderBlock r = [] : renderMutationRecord r
   in header : countLine : concatMap renderBlock records
  where
    pluralise n word = if n == 1 then word else word ++ "s"

-- | Write the rendered manifest to @<dir>/<moduleName>.txt@ with
-- 8-bit ANSI escapes embedded.  Reviewers can read with @cat foo.txt@ or
-- @less -R foo.txt@.
--
-- An empty manifest still produces a file (containing just the module
-- header) so the directory keeps a 1:1 correspondence between @.json@ and
-- @.txt@ entries.
writeManifestTxtFile :: Path Abs Dir -> String -> MutationManifest -> IO ()
writeManifestTxtFile dir moduleName manifest = do
  ensureDir dir
  fileName <- parseRelFile (moduleName ++ ".txt")
  let rendered = renderManifest moduleName manifest
      txt = renderChunksText With8BitColours (unlinesChunks rendered)
      path = fromAbsFile (dir </> fileName)
  IO.withFile path IO.WriteMode $ \h -> do
    IO.hSetBinaryMode h True
    IO.hPutStr h (T.unpack txt)

-- ---------------------------------------------------------------------------
-- Diff rendering (shared with sydtest's MutationMode.Common)

-- | Render a unified-diff hunk with intra-line colouring.  Used by both
-- 'renderMutationRecord' and the runtime mutation report.
--
-- @startLine@ is the 1-based source line of the first 'srcLines' entry;
-- the @@\@\@ -a,b +c,d \@\@@ header is computed from there and the lengths
-- of context, source, and mutated lines.
renderUnifiedDiff :: Int -> [Text] -> [Text] -> [Text] -> [Text] -> [[Chunk]]
renderUnifiedDiff startLine ctxBefore srcLines mutLines ctxAfter =
  let allBefore = ctxBefore ++ srcLines ++ ctxAfter
      allAfter = ctxBefore ++ mutLines ++ ctxAfter
      groups = getGroupedDiff allBefore allAfter
      hunkStart = startLine - length ctxBefore
      origCount = length allBefore
      mutCount = length allAfter
      hunkHeader =
        T.pack $
          "@@ -"
            ++ show hunkStart
            ++ ","
            ++ show origCount
            ++ " +"
            ++ show hunkStart
            ++ ","
            ++ show mutCount
            ++ " @@"
   in [fore cyan (chunk hunkHeader)] : renderGroups groups
  where
    renderGroups :: [PolyDiff [Text] [Text]] -> [[Chunk]]
    renderGroups [] = []
    renderGroups (Both ls _ : rest) =
      map (\l -> [chunk (T.cons ' ' l)]) ls ++ renderGroups rest
    renderGroups gs@(First _ : _) =
      let (dels, adds, rest) = collectChange gs
       in renderPaired dels adds ++ renderGroups rest
    renderGroups gs@(Second _ : _) =
      let (dels, adds, rest) = collectChange gs
       in renderPaired dels adds ++ renderGroups rest

    collectChange :: [PolyDiff [Text] [Text]] -> ([Text], [Text], [PolyDiff [Text] [Text]])
    collectChange = go [] []
      where
        go ds as (First ls : rest) = go (ds ++ ls) as rest
        go ds as (Second ls : rest) = go ds (as ++ ls) rest
        go ds as rest = (ds, as, rest)

    renderDelLine :: Text -> [Chunk]
    renderDelLine l = [fore delColour (chunk (T.cons '-' l))]

    renderAddLine :: Text -> [Chunk]
    renderAddLine l = [fore addColour (chunk (T.cons '+' l))]

    renderPaired :: [Text] -> [Text] -> [[Chunk]]
    renderPaired dels adds =
      let n = min (length dels) (length adds)
          (pairedDels, extraDels) = splitAt n dels
          (pairedAdds, extraAdds) = splitAt n adds
          (delLines, addLines) = unzip (zipWith renderPair pairedDels pairedAdds)
       in delLines ++ map renderDelLine extraDels ++ addLines ++ map renderAddLine extraAdds

    renderPair :: Text -> Text -> ([Chunk], [Chunk])
    renderPair delLine addLine =
      let charDiff = V.toList (getTextDiff delLine addLine)
          delChunks = fore delColour (chunk (T.singleton '-')) : renderDelSide charDiff
          addChunks = fore addColour (chunk (T.singleton '+')) : renderAddSide charDiff
       in (delChunks, addChunks)

-- ---------------------------------------------------------------------------
-- Colour helpers (formerly in sydtest's Test.Syd.Output.Common; moved here
-- so the runtime can render manifest diffs without depending on sydtest).

-- | Foreground colour used on deletion lines (the "@-@" side of a diff).
-- Plain 'red' rather than a 256-colour shade so each terminal applies its
-- own palette.
delColour :: Colour
delColour = red

-- | Foreground colour used on addition lines (the "@+@" side).
addColour :: Colour
addColour = green

-- | Emphasise an intra-line changed substring.  Non-whitespace text gets
-- bold + the brighter shade of the side's colour, so it stands out within
-- a line that is otherwise foreground-coloured with the dull shade.
-- Whitespace-only text has no glyph to colour, so fill the background
-- instead.
emphasiseIntraLine :: Colour -> Colour -> Text -> Chunk
emphasiseIntraLine lineCol brightCol t =
  if T.null (T.strip t)
    then back lineCol (chunk t)
    else bold (fore brightCol (chunk t))

-- | Render the deletion side of a character-level diff.  'Both' chars get
-- 'delColour' as their whole-line foreground; this-side changes ('First')
-- get 'emphasiseIntraLine'd with 'brightRed'.  Addition-only chunks
-- ('Second') are dropped — they belong to the other side's line.
renderDelSide :: [PolyDiff Text Text] -> [Chunk]
renderDelSide =
  mapMaybe $ \case
    First t -> Just (emphasiseIntraLine delColour brightRed t)
    Second _ -> Nothing
    Both t _ -> Just (fore delColour (chunk t))

-- | Symmetric counterpart of 'renderDelSide' for the addition side.
renderAddSide :: [PolyDiff Text Text] -> [Chunk]
renderAddSide =
  mapMaybe $ \case
    First _ -> Nothing
    Second t -> Just (emphasiseIntraLine addColour brightGreen t)
    Both t _ -> Just (fore addColour (chunk t))
