{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Output.Common where

import Control.Exception
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (cast)
import Data.Word
import Myers.Diff
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Colour
import Text.Printf

padding :: Chunk
padding = chunk $ T.replicate paddingSize " "

paddingSize :: Int
paddingSize = 2

orange :: Colour
orange = colour256 166

darkRed :: Colour
darkRed = colour256 160

statusColour :: TestStatus -> Colour
statusColour = \case
  TestPassed -> green
  TestFailed -> red

statusCheckMark :: TestStatus -> Text
statusCheckMark = \case
  TestPassed -> "\10003 "
  TestFailed -> "\10007 "

timeChunkFor :: Word64 -> Chunk
timeChunkFor executionTime =
  let t = fromIntegral executionTime / 1_000_000 :: Double -- milliseconds
      executionTimeText = T.pack (printf "%10.2f ms" t)
      withTimingColour =
        if
          | t < 10 -> fore green
          | t < 100 -> fore yellow
          | t < 1_000 -> fore orange
          | t < 10_000 -> fore red
          | otherwise -> fore darkRed
   in withTimingColour $ chunk executionTimeText

stringChunks :: String -> [[Chunk]]
stringChunks s =
  let ls = lines s
   in map ((: []) . chunk . T.pack) ls

indexed :: [a] -> (Word -> a -> b) -> [b]
indexed ls func = zipWith func [1 ..] ls

commaList :: [String] -> String
commaList [] = []
commaList [s] = s
commaList (s1 : rest) = s1 ++ ", " ++ commaList rest

mContextChunks :: Maybe String -> [[Chunk]]
mContextChunks = maybe [] stringChunks

outputSomeException :: SomeException -> [[Chunk]]
outputSomeException outerException =
  case fromException outerException :: Maybe Contextual of
    Just (Contextual innerException s) ->
      -- Check if innerException is already a SomeException to avoid double-wrapping
      let innerSE = case cast innerException of
            Just se -> se :: SomeException
            Nothing -> SomeException innerException
       in outputSomeException innerSE ++ stringChunks s
    Nothing ->
      case fromException outerException :: Maybe Assertion of
        Just a -> outputAssertion a
        Nothing -> stringChunks $ displayException outerException

outputAssertion :: Assertion -> [[Chunk]]
outputAssertion = \case
  NotEqualButShouldHaveBeenEqualWithDiff actual expected diffM -> outputEqualityAssertionFailed actual expected diffM
  EqualButShouldNotHaveBeenEqual actual notExpected -> outputNotEqualAssertionFailed actual notExpected
  PredicateFailedButShouldHaveSucceeded actual mName -> outputPredicateSuccessAssertionFailed actual mName
  PredicateSucceededButShouldHaveFailed actual mName -> outputPredicateFailAssertionFailed actual mName
  ExpectationFailed s -> stringChunks s
  Context a' context -> outputAssertion a' ++ stringChunks context

-- | Split a list of 'Chunk's into lines of [Chunks].
--
-- This is rather complicated because chunks may contain newlines, in which
-- case they need to be split into two chunks on separate lines but with the
-- same colour information.
-- However, separate chunks are not necessarily on separate lines because there
-- may not be a newline inbetween.
splitChunksIntoLines :: [Chunk] -> [[Chunk]]
splitChunksIntoLines =
  -- We maintain a list of 'currently traversing lines'.
  -- These are already split into newlines and therefore definitely belong on separate lines.
  -- We still need to keep the last of the current line though, because it
  -- does not end in a newline and should therefore not necessarily belong on
  -- a separate line by itself.
  go ([] :| []) -- Start with an empty current line.
  where
    -- CurrentlyTraversingLines -> ChunksToStillSplit -> SplitChunks
    go :: NonEmpty [Chunk] -> [Chunk] -> [[Chunk]]
    go cls cs = case NE.uncons cls of
      (currentLine, mRest) -> case mRest of
        -- If there's only one current line, that's the last one of the currently traversing lines.
        -- We split the next chunk into lines and append the first line of that to the current line.
        Nothing -> case cs of
          -- If there is only one current line, and no more chunks, it's the last line.
          [] -> [currentLine]
          -- If there are chunks left, split the first one into lines.
          (c : rest) -> case T.splitOn "\n" (chunkText c) of
            -- Should not happen, but would be fine, just skip this chunk
            [] -> go cls rest
            -- If the chunk had more than one lines
            (l : ls) -> case NE.nonEmpty ls of
              -- If there was only one line in the chunk, we continue with the
              -- same current line onto the rest of the chunks
              Nothing -> go ((currentLine <> [c {chunkText = l}]) :| []) rest
              -- If there was more than one line in that chunk, that line is now considered finished.
              -- We then make all the lines of this new chunk the new current lines, one chunk per line.
              Just ne -> (currentLine <> [c {chunkText = l}]) : go (NE.map (\l' -> [c {chunkText = l'}]) ne) rest
        -- If there is more than one current line, all but the last one are considered finished.
        -- We skip them one by one.
        Just ne -> currentLine : go ne cs

outputEqualityAssertionFailed :: String -> String -> Maybe [PolyDiff Text Text] -> [[Chunk]]
outputEqualityAssertionFailed actual expected diffM =
  case diffM of
    Just diff -> formatDiff actual expected diff
    Nothing ->
      concat
        [ [[chunk "Expected these values to be equal:"]],
          [[chunk "Diff computation took too long and was canceled"]],
          [[fromString actual]],
          [[fromString expected]]
        ]

formatDiff :: String -> String -> [PolyDiff Text Text] -> [[Chunk]]
formatDiff actual expected diff =
  let -- Add a header to a list of lines of chunks
      chunksLinesWithHeader :: Chunk -> [[Chunk]] -> [[Chunk]]
      chunksLinesWithHeader header = \case
        -- If there is only one line, put the header on that line.
        [cs] -> [header : cs]
        -- If there is more than one line, put the header on a separate line before
        cs -> [header] : cs

      -- If it's only whitespace, change the background, otherwise change the foreground
      foreOrBack :: Colour -> Text -> Chunk
      foreOrBack c t =
        (if T.null (T.strip t) then back c else fore c)
          (chunk t)
      actualChunks :: [[Chunk]]
      actualChunks = chunksLinesWithHeader (fore blue "Actual:   ") $
        splitChunksIntoLines $
          flip mapMaybe diff $ \case
            First t -> Just $ foreOrBack red t
            Second _ -> Nothing
            Both t _ -> Just $ chunk t
      expectedChunks :: [[Chunk]]
      expectedChunks = chunksLinesWithHeader (fore blue "Expected: ") $
        splitChunksIntoLines $
          flip mapMaybe diff $ \case
            First _ -> Nothing
            Second t -> Just $ foreOrBack green t
            Both t _ -> Just $ chunk t
      inlineDiffChunks :: [[Chunk]]
      inlineDiffChunks =
        if length (lines actual) == 1 && length (lines expected) == 1
          then []
          else chunksLinesWithHeader (fore blue "Inline diff: ") $
            splitChunksIntoLines $
              flip map diff $ \case
                First t -> foreOrBack red t
                Second t -> foreOrBack green t
                Both t _ -> chunk t
   in concat
        [ [[chunk "Expected these values to be equal:"]],
          actualChunks,
          expectedChunks,
          inlineDiffChunks
        ]

outputNotEqualAssertionFailed :: String -> String -> [[Chunk]]
outputNotEqualAssertionFailed actual notExpected =
  if actual == notExpected -- String equality
    then
      [ [chunk "Did not expect equality of the values but both were:"],
        [chunk (T.pack actual)]
      ]
    else
      [ [chunk "These two values were considered equal but should not have been equal:"],
        [fore blue "Actual      : ", chunk (T.pack actual)],
        [fore blue "Not Expected: ", chunk (T.pack notExpected)]
      ]

outputPredicateSuccessAssertionFailed :: String -> Maybe String -> [[Chunk]]
outputPredicateSuccessAssertionFailed actual mName =
  concat
    [ [ [chunk "Predicate failed, but should have succeeded, on this value:"],
        [chunk (T.pack actual)]
      ],
      concat [map (chunk "Predicate: " :) (stringChunks name) | name <- maybeToList mName]
    ]

outputPredicateFailAssertionFailed :: String -> Maybe String -> [[Chunk]]
outputPredicateFailAssertionFailed actual mName =
  concat
    [ [ [chunk "Predicate succeeded, but should have failed, on this value:"],
        [chunk (T.pack actual)]
      ],
      concat [map (chunk "Predicate: " :) (stringChunks name) | name <- maybeToList mName]
    ]

resultForestWidth :: SpecForest a -> Int
resultForestWidth = goF 0
  where
    goF :: Int -> SpecForest a -> Int
    goF level = maximum . map (goT level)
    goT :: Int -> SpecTree a -> Int
    goT level = \case
      SpecifyNode t _ -> T.length t + level * paddingSize
      PendingNode t _ -> T.length t + level * paddingSize
      DescribeNode _ sdf -> goF (succ level) sdf
      SubForestNode sdf -> goF level sdf

specForestWidth :: SpecDefForest a b c -> Int
specForestWidth = goF 0
  where
    goF :: Int -> SpecDefForest a b c -> Int
    goF level = \case
      [] -> 0
      ts -> maximum $ map (goT level) ts
    goT :: Int -> SpecDefTree a b c -> Int
    goT level = \case
      DefSpecifyNode t _ _ -> T.length t + level * paddingSize
      DefPendingNode t _ -> T.length t + level * paddingSize
      DefDescribeNode _ sdf -> goF (succ level) sdf
      DefSetupNode _ sdf -> goF level sdf
      DefBeforeAllNode _ sdf -> goF level sdf
      DefBeforeAllWithNode _ sdf -> goF level sdf
      DefWrapNode _ sdf -> goF level sdf
      DefAroundAllNode _ sdf -> goF level sdf
      DefAroundAllWithNode _ sdf -> goF level sdf
      DefAfterAllNode _ sdf -> goF level sdf
      DefParallelismNode _ sdf -> goF level sdf
      DefTimeoutNode _ sdf -> goF level sdf
      DefRetriesNode _ sdf -> goF level sdf
      DefRandomisationNode _ sdf -> goF level sdf
      DefFlakinessNode _ sdf -> goF level sdf
      DefExpectationNode _ sdf -> goF level sdf
