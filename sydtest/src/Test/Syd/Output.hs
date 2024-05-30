{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Output where

import Control.Arrow (second)
import Control.Exception
import Data.List (sortOn)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Vector as V
import Data.Word
import GHC.Stack
import Myers.Diff
import Safe
import Test.QuickCheck.IO ()
import Test.Syd.OptParse
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Colour
import Text.Printf

printOutputSpecForest :: Settings -> Timed ResultForest -> IO ()
printOutputSpecForest settings results = do
  tc <- deriveTerminalCapababilities settings
  LTIO.putStr $ LTB.toLazyText $ renderResultReport settings tc results

renderResultReport :: Settings -> TerminalCapabilities -> Timed ResultForest -> Text.Builder
renderResultReport settings tc rf =
  mconcat $
    map
      (\line -> renderChunksBuilder tc line <> "\n")
      (outputResultReport settings rf)

outputResultReport :: Settings -> Timed ResultForest -> [[Chunk]]
outputResultReport settings trf =
  let rf = timedValue trf
   in concat
        [ outputTestsHeader,
          outputSpecForest settings 0 (resultForestWidth rf) rf,
          [ [chunk ""],
            [chunk ""]
          ],
          outputFailuresWithHeading settings rf,
          [[chunk ""]],
          outputStats (computeTestSuiteStats settings <$> trf),
          [[chunk ""]],
          if settingProfile settings
            then outputProfilingInfo trf
            else []
        ]

outputFailuresHeader :: [[Chunk]]
outputFailuresHeader = outputHeader "Failures:"

outputFailuresWithHeading :: Settings -> ResultForest -> [[Chunk]]
outputFailuresWithHeading settings rf =
  if shouldExitFail settings rf
    then
      concat
        [ outputFailuresHeader,
          outputFailures settings rf
        ]
    else []

outputStats :: Timed TestSuiteStats -> [[Chunk]]
outputStats timed =
  let TestSuiteStats {..} = timedValue timed
      sumTimeSeconds :: Double
      sumTimeSeconds = fromIntegral testSuiteStatSumTime / 1_000_000_000
      totalTimeSeconds :: Double
      totalTimeSeconds = fromIntegral (timedTime timed) / 1_000_000_000
   in map (padding :) $
        concat
          [ [ [ chunk "Examples:                     ",
                fore green $ chunk (T.pack (show testSuiteStatExamples))
              ]
              | testSuiteStatExamples /= testSuiteStatSuccesses
            ],
            [ [ chunk "Passed:                       ",
                fore green $ chunk (T.pack (show testSuiteStatSuccesses))
              ],
              [ chunk "Failed:                       ",
                ( if testSuiteStatFailures > 0
                    then fore red
                    else fore green
                )
                  $ chunk (T.pack (show testSuiteStatFailures))
              ]
            ],
            [ [ chunk "Flaky:                        ",
                fore red $ chunk (T.pack (show testSuiteStatFlakyTests))
              ]
              | testSuiteStatFlakyTests > 0
            ],
            [ [ chunk "Pending:                      ",
                fore magenta $ chunk (T.pack (show testSuiteStatPending))
              ]
              | testSuiteStatPending > 0
            ],
            [ [ chunk "Sum of test runtimes:",
                fore yellow $ chunk $ T.pack (printf "%13.2f seconds" sumTimeSeconds)
              ],
              [ chunk "Test suite took:     ",
                fore yellow $ chunk $ T.pack (printf "%13.2f seconds" totalTimeSeconds)
              ]
            ]
          ]

outputProfilingInfo :: Timed ResultForest -> [[Chunk]]
outputProfilingInfo Timed {..} =
  map
    ( \(path, nanos) ->
        [ timeChunkFor nanos,
          " ",
          chunk $ T.intercalate "." path
        ]
    )
    ( sortOn
        snd
        ( map
            (second (timedTime . testDefVal))
            (flattenSpecForest timedValue)
        )
    )

outputTestsHeader :: [[Chunk]]
outputTestsHeader = outputHeader "Tests:"

outputHeader :: Text -> [[Chunk]]
outputHeader t =
  [ [fore blue $ chunk t],
    [chunk ""]
  ]

outputSpecForest :: Settings -> Int -> Int -> ResultForest -> [[Chunk]]
outputSpecForest settings level treeWidth = concatMap (outputSpecTree settings level treeWidth)

outputSpecTree :: Settings -> Int -> Int -> ResultTree -> [[Chunk]]
outputSpecTree settings level treeWidth = \case
  SpecifyNode t td -> outputSpecifyLines settings level treeWidth t td
  PendingNode t mr -> outputPendingLines t mr
  DescribeNode t sf -> outputDescribeLine t : map (padding :) (outputSpecForest settings (level + 1) treeWidth sf)
  SubForestNode sf -> outputSpecForest settings level treeWidth sf

outputDescribeLine :: Text -> [Chunk]
outputDescribeLine t = [fore yellow $ chunk t]

outputSpecifyLines :: Settings -> Int -> Int -> Text -> TDef (Timed TestRunReport) -> [[Chunk]]
outputSpecifyLines settings level treeWidth specifyText (TDef timed _) =
  let testRunReport = timedValue timed
      executionTime = timedTime timed
      status = testRunReportStatus settings testRunReport
      TestRunResult {..} = testRunReportReportedRun testRunReport
      withStatusColour = fore (statusColour status)
      pad = (chunk (T.pack (replicate paddingSize ' ')) :)
      timeChunk = timeChunkFor executionTime
   in filter
        (not . null)
        $ concat
          [ [ [ withStatusColour $ chunk (statusCheckMark status),
                withStatusColour $ chunk specifyText,
                spacingChunk level specifyText (chunkText timeChunk) treeWidth,
                timeChunk
              ]
            ],
            map pad $ retriesChunks testRunReport,
            [ pad
                [ chunk "passed for all of ",
                  case w of
                    0 -> fore red $ chunk "0"
                    _ -> fore green $ chunk (T.pack (printf "%d" w)),
                  " inputs."
                ]
              | status == TestPassed,
                w <- maybeToList testRunResultNumTests
            ],
            map pad $ labelsChunks (fromMaybe 1 testRunResultNumTests) testRunResultLabels,
            map pad $ classesChunks testRunResultClasses,
            map pad $ tablesChunks testRunResultTables,
            [pad $ outputGoldenCase gc | gc <- maybeToList testRunResultGoldenCase]
          ]

exampleNrChunk :: Word -> Word -> Chunk
exampleNrChunk total current =
  let digits :: Word
      digits = max 2 $ succ $ floor $ logBase 10 $ (fromIntegral :: Word -> Double) total
      formatStr = "%" <> show digits <> "d"
   in chunk $ T.pack $ printf formatStr current

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

retriesChunks :: TestRunReport -> [[Chunk]]
retriesChunks testRunReport =
  case testRunReportRetries testRunReport of
    Nothing -> []
    Just retries ->
      let flaky = testRunReportWasFlaky testRunReport
          mMessage = case testRunReportFlakinessMode testRunReport of
            MayBeFlaky mmesg -> mmesg
            MayNotBeFlaky -> Nothing
       in if flaky
            then
              concat
                [ [["Retries: ", chunk (T.pack (show retries)), fore red " !!! FLAKY !!!"]],
                  [[fore magenta $ chunk $ T.pack message] | message <- maybeToList mMessage]
                ]
            else [["Retries: ", chunk (T.pack (show retries)), " (does not look flaky)"]]

labelsChunks :: Word -> Maybe (Map [String] Int) -> [[Chunk]]
labelsChunks _ Nothing = []
labelsChunks totalCount (Just labels)
  | M.null labels = []
  | map fst (M.toList labels) == [[]] = []
  | otherwise =
      [chunk "Labels"]
        : map
          ( pad
              . ( \(ss, i) ->
                    [ chunk
                        ( T.pack
                            ( printf
                                "%5.2f%% %s"
                                (100 * fromIntegral i / fromIntegral totalCount :: Double)
                                (commaList (map show ss))
                            )
                        )
                    ]
                )
          )
          (M.toList labels)
  where
    pad = (chunk (T.pack (replicate paddingSize ' ')) :)

classesChunks :: Maybe (Map String Int) -> [[Chunk]]
classesChunks Nothing = []
classesChunks (Just classes)
  | M.null classes = []
  | otherwise =
      [chunk "Classes"]
        : map
          ( pad
              . ( \(s, i) ->
                    [ chunk
                        ( T.pack
                            ( printf "%5.2f%% %s" (100 * fromIntegral i / fromIntegral total :: Double) s
                            )
                        )
                    ]
                )
          )
          (M.toList classes)
  where
    pad = (chunk (T.pack (replicate paddingSize ' ')) :)
    total = sum $ map snd $ M.toList classes

tablesChunks :: Maybe (Map String (Map String Int)) -> [[Chunk]]
tablesChunks Nothing = []
tablesChunks (Just tables) = concatMap (uncurry goTable) $ M.toList tables
  where
    goTable :: String -> Map String Int -> [[Chunk]]
    goTable tableName percentages =
      [chunk " "]
        : [chunk (T.pack tableName)]
        : map
          ( pad
              . ( \(s, i) ->
                    [ chunk
                        ( T.pack
                            ( printf "%5.2f%% %s" (100 * fromIntegral i / fromIntegral total :: Double) s
                            )
                        )
                    ]
                )
          )
          (M.toList percentages)
      where
        pad = (chunk (T.pack (replicate paddingSize ' ')) :)
        total = sum $ map snd $ M.toList percentages

outputPendingLines :: Text -> Maybe Text -> [[Chunk]]
outputPendingLines specifyText mReason =
  filter
    (not . null)
    [ [fore magenta $ chunk specifyText],
      case mReason of
        Nothing -> []
        Just reason -> [padding, chunk reason]
    ]

outputFailureLabels :: Maybe (Map [String] Int) -> [[Chunk]]
outputFailureLabels Nothing = []
outputFailureLabels (Just labels)
  | labels == M.singleton [] 1 = []
  | otherwise = [["Labels: ", chunk (T.pack (commaList (map show (concat $ M.keys labels))))]]

commaList :: [String] -> String
commaList [] = []
commaList [s] = s
commaList (s1 : rest) = s1 ++ ", " ++ commaList rest

outputFailureClasses :: Maybe (Map String Int) -> [[Chunk]]
outputFailureClasses Nothing = []
outputFailureClasses (Just classes)
  | M.null classes = []
  | otherwise = [["Class: ", chunk (T.pack (commaList (M.keys classes)))]]

outputGoldenCase :: GoldenCase -> [Chunk]
outputGoldenCase = \case
  GoldenNotFound -> [fore red $ chunk "Golden output not found"]
  GoldenStarted -> [fore cyan $ chunk "Golden output created"]
  GoldenReset -> [fore cyan $ chunk "Golden output reset"]

-- The chunk for spacing between the description and the timing
--
-- initial padding | checkmark | description | THIS CHUNK | execution time
spacingChunk :: Int -> Text -> Text -> Int -> Chunk
spacingChunk level descriptionText executionTimeText treeWidth = chunk $ T.pack $ replicate paddingWidth ' '
  where
    paddingWidth =
      let preferredMaxWidth = 80
          checkmarkWidth = 2
          minimumSpacing = 1
          actualDescriptionWidth = T.length descriptionText
          actualTimingWidth = T.length executionTimeText
          totalNecessaryWidth = treeWidth + checkmarkWidth + minimumSpacing + actualTimingWidth -- All timings are the same width
          actualMaxWidth = max totalNecessaryWidth preferredMaxWidth
       in actualMaxWidth - paddingSize * level - actualTimingWidth - actualDescriptionWidth

outputFailures :: Settings -> ResultForest -> [[Chunk]]
outputFailures settings rf =
  let failures = filter (testRunReportFailed settings . timedValue . testDefVal . snd) $ flattenSpecForest rf
      nbDigitsInFailureCount :: Int
      nbDigitsInFailureCount = floor (logBase 10 (L.genericLength failures) :: Double)
      padFailureDetails = (chunk (T.pack (replicate (nbDigitsInFailureCount + 4) ' ')) :)
   in map (padding :) $
        filter (not . null) $
          concat $
            indexed failures $ \w (ts, TDef timed cs) ->
              let testRunReport = timedValue timed
                  status = testRunReportStatus settings testRunReport
                  TestRunResult {..} = testRunReportReportedRun testRunReport
               in concat
                    [ [ [ fore cyan $
                            chunk $
                              T.pack $
                                replicate 2 ' '
                                  ++ case headMay $ getCallStack cs of
                                    Nothing -> "Unknown location"
                                    Just (_, SrcLoc {..}) ->
                                      concat
                                        [ srcLocFile,
                                          ":",
                                          show srcLocStartLine
                                        ]
                        ],
                        map
                          (fore (statusColour status))
                          [ chunk $ statusCheckMark status,
                            chunk $ T.pack (printf ("%" ++ show nbDigitsInFailureCount ++ "d ") w),
                            chunk $ T.intercalate "." ts
                          ]
                      ],
                      map padFailureDetails $ retriesChunks testRunReport,
                      map (padFailureDetails . (: []) . chunk . T.pack) $
                        case (testRunResultNumTests, testRunResultNumShrinks) of
                          (Nothing, _) -> []
                          (Just numTests, Nothing) -> [printf "Failed after %d tests" numTests]
                          (Just numTests, Just 0) -> [printf "Failed after %d tests" numTests]
                          (Just numTests, Just numShrinks) -> [printf "Failed after %d tests and %d shrinks" numTests numShrinks],
                      map (padFailureDetails . (\c -> [chunk "Generated: ", c]) . fore yellow . chunk . T.pack) testRunResultFailingInputs,
                      map padFailureDetails $ outputFailureLabels testRunResultLabels,
                      map padFailureDetails $ outputFailureClasses testRunResultClasses,
                      map padFailureDetails $ maybe [] outputSomeException testRunResultException,
                      [padFailureDetails $ outputGoldenCase gc | gc <- maybeToList testRunResultGoldenCase],
                      concat [map padFailureDetails $ stringChunks ei | ei <- maybeToList testRunResultExtraInfo],
                      [[chunk ""]]
                    ]

outputSomeException :: SomeException -> [[Chunk]]
outputSomeException outerException =
  case fromException outerException :: Maybe Contextual of
    Just (Contextual innerException s) -> outputSomeException (SomeException innerException) ++ stringChunks s
    Nothing ->
      case fromException outerException :: Maybe Assertion of
        Just a -> outputAssertion a
        Nothing -> stringChunks $ displayException outerException

outputAssertion :: Assertion -> [[Chunk]]
outputAssertion = \case
  NotEqualButShouldHaveBeenEqual actual expected -> outputEqualityAssertionFailed actual expected
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

outputEqualityAssertionFailed :: String -> String -> [[Chunk]]
outputEqualityAssertionFailed actual expected =
  let diff = V.toList $ getTextDiff (T.pack actual) (T.pack expected)
      -- Add a header to a list of lines of chunks
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

mContextChunks :: Maybe String -> [[Chunk]]
mContextChunks = maybe [] stringChunks

stringChunks :: String -> [[Chunk]]
stringChunks s =
  let ls = lines s
   in map ((: []) . chunk . T.pack) ls

indexed :: [a] -> (Word -> a -> b) -> [b]
indexed ls func = zipWith func [1 ..] ls

statusColour :: TestStatus -> Colour
statusColour = \case
  TestPassed -> green
  TestFailed -> red

statusCheckMark :: TestStatus -> Text
statusCheckMark = \case
  TestPassed -> "\10003 "
  TestFailed -> "\10007 "

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
      DefRetriesNode _ sdf -> goF level sdf
      DefRandomisationNode _ sdf -> goF level sdf
      DefFlakinessNode _ sdf -> goF level sdf
      DefExpectationNode _ sdf -> goF level sdf

padding :: Chunk
padding = chunk $ T.replicate paddingSize " "

paddingSize :: Int
paddingSize = 2

orange :: Colour
orange = colour256 166

darkRed :: Colour
darkRed = colour256 160
