{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Output where

import Control.Monad.Reader
import Data.Algorithm.Diff
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Char8 as SB8
import qualified Data.List as L
import Data.List.Split (splitWhen)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Safe
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Colour
import Text.Printf

#ifdef mingw32_HOST_OS
import System.Console.ANSI (hSupportsANSIColor)
import System.IO (stdout)
#else
import Text.Colour.Capabilities.FromEnv
#endif

printOutputSpecForest :: TerminalCapabilities -> Timed ResultForest -> IO ()
printOutputSpecForest tc results = do
  forM_ (outputResultReport results) $ \chunks -> do
    putChunksWith tc chunks
    SB8.putStrLn ""

renderResultReport :: TerminalCapabilities -> Timed ResultForest -> Builder
renderResultReport tc rf =
  mconcat $
    L.intersperse (SBB.char7 '\n') $
      map (renderChunks tc) (outputResultReport rf)

outputResultReport :: Timed ResultForest -> [[Chunk]]
outputResultReport trf@(Timed rf _) =
  concat
    [ outputTestsHeader,
      outputSpecForest 0 (resultForestWidth rf) rf,
      [ [chunk ""],
        [chunk ""]
      ],
      outputFailuresWithHeading rf,
      [[chunk ""]],
      outputStats (computeTestSuiteStats <$> trf),
      [[chunk ""]]
    ]

outputFailuresHeader :: [[Chunk]]
outputFailuresHeader = outputHeader "Failures:"

outputFailuresWithHeading :: ResultForest -> [[Chunk]]
outputFailuresWithHeading rf =
  if any testFailed (flattenSpecForest rf)
    then
      concat
        [ outputFailuresHeader,
          outputFailures rf
        ]
    else []

outputStats :: Timed TestSuiteStats -> [[Chunk]]
outputStats (Timed TestSuiteStats {..} timing) =
  let sumTimeSeconds :: Double
      sumTimeSeconds = fromIntegral testSuiteStatSumTime / 1_000_000_000
      totalTimeSeconds :: Double
      totalTimeSeconds = fromIntegral timing / 1_000_000_000
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
            concat
              [ let longestTimeSeconds :: Double
                    longestTimeSeconds = fromIntegral longestTestTime / 1_000_000_000
                    longestTimePercentage :: Double
                    longestTimePercentage = 100 * longestTimeSeconds / sumTimeSeconds
                    showLongestTestDetails = longestTimePercentage > 50
                 in filter
                      (not . null)
                      [ concat
                          [ [ "Longest test:                 ",
                              fore green $ chunk longestTestName
                            ]
                            | showLongestTestDetails
                          ],
                        concat
                          [ [ chunk "Longest test took:   ",
                              fore yellow $ chunk $ T.pack (printf "%13.2f seconds" longestTimeSeconds)
                            ],
                            [ chunk $ T.pack (printf ", which is %.0f%% of total runtime" longestTimePercentage)
                              | showLongestTestDetails
                            ]
                          ]
                      ]
                | (longestTestName, longestTestTime) <- maybeToList testSuiteStatLongestTime
              ],
            [ [ chunk "Sum of test runtimes:",
                fore yellow $ chunk $ T.pack (printf "%13.2f seconds" sumTimeSeconds)
              ],
              [ chunk "Test suite took:     ",
                fore yellow $ chunk $ T.pack (printf "%13.2f seconds" totalTimeSeconds)
              ]
            ]
          ]

outputTestsHeader :: [[Chunk]]
outputTestsHeader = outputHeader "Tests:"

outputHeader :: Text -> [[Chunk]]
outputHeader t =
  [ [fore blue $ chunk t],
    [chunk ""]
  ]

outputSpecForest :: Int -> Int -> ResultForest -> [[Chunk]]
outputSpecForest level treeWidth = concatMap (outputSpecTree level treeWidth)

outputSpecTree :: Int -> Int -> ResultTree -> [[Chunk]]
outputSpecTree level treeWidth = \case
  SpecifyNode t td -> outputSpecifyLines level treeWidth t td
  PendingNode t mr -> outputPendingLines t mr
  DescribeNode t sf -> outputDescribeLine t : map (padding :) (outputSpecForest (level + 1) treeWidth sf)
  SubForestNode sf -> outputSpecForest level treeWidth sf

outputDescribeLine :: Text -> [Chunk]
outputDescribeLine t = [fore yellow $ chunk t]

outputSpecifyLines :: Int -> Int -> Text -> TDef (Timed TestRunResult) -> [[Chunk]]
outputSpecifyLines level treeWidth specifyText (TDef (Timed TestRunResult {..} executionTime) _) =
  let t = fromIntegral executionTime / 1_000_000 :: Double -- milliseconds
      executionTimeText = T.pack (printf "%10.2f ms" t)
      withTimingColour =
        if
            | t < 10 -> fore green
            | t < 100 -> fore yellow
            | t < 1000 -> fore orange
            | t < 10000 -> fore red
            | otherwise -> fore darkRed

      withStatusColour = fore (statusColour testRunResultStatus)
      pad = (chunk (T.pack (replicate paddingSize ' ')) :)
   in filter
        (not . null)
        $ concat
          [ [ [ withStatusColour $ chunk (statusCheckMark testRunResultStatus),
                withStatusColour $ chunk specifyText,
                spacingChunk level specifyText executionTimeText treeWidth,
                withTimingColour $ chunk executionTimeText
              ]
            ],
            map pad $ retriesChunks testRunResultStatus testRunResultRetries testRunResultFlakinessMessage,
            [ pad
                [ chunk "passed for all of ",
                  case w of
                    0 -> fore red $ chunk "0"
                    _ -> fore green $ chunk (T.pack (printf "%d" w)),
                  " inputs."
                ]
              | testRunResultStatus == TestPassed,
                w <- maybeToList testRunResultNumTests
            ],
            map pad $ labelsChunks testRunResultLabels,
            map pad $ classesChunks testRunResultClasses,
            map pad $ tablesChunks testRunResultTables,
            [pad $ outputGoldenCase gc | gc <- maybeToList testRunResultGoldenCase]
          ]

retriesChunks :: TestStatus -> Maybe Int -> Maybe String -> [[Chunk]]
retriesChunks status mRetries mMessage = case mRetries of
  Nothing -> []
  Just retries -> case status of
    TestPassed ->
      concat
        [ [["Retries: ", chunk (T.pack (show retries)), fore red " !!! FLAKY !!!"]],
          [[fore magenta $ chunk $ T.pack message] | message <- maybeToList mMessage]
        ]
    TestFailed -> [["Retries: ", chunk (T.pack (show retries)), " (likely not flaky)"]]

labelsChunks :: Maybe (Map [String] Int) -> [[Chunk]]
labelsChunks Nothing = []
labelsChunks (Just labels)
  | M.size labels <= 1 = []
  | otherwise =
    [chunk "labels"] :
    map
      ( pad
          . ( \(ss, i) ->
                [ chunk
                    ( T.pack
                        ( printf "%5.2f%% %s" (100 * fromIntegral i / fromIntegral total :: Double) (commaList (map show ss))
                        )
                    )
                ]
            )
      )
      (M.toList labels)
  where
    pad = (chunk (T.pack (replicate paddingSize ' ')) :)
    total = sum $ map snd $ M.toList labels

classesChunks :: Maybe (Map String Int) -> [[Chunk]]
classesChunks Nothing = []
classesChunks (Just classes)
  | M.null classes = []
  | otherwise =
    [chunk "classes"] :
    map
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
      [chunk " "] :
      [chunk (T.pack tableName)] :
      map
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

testFailed :: (a, TDef (Timed TestRunResult)) -> Bool
testFailed = (== TestFailed) . testRunResultStatus . timedValue . testDefVal . snd

outputFailures :: ResultForest -> [[Chunk]]
outputFailures rf =
  let failures = filter testFailed $ flattenSpecForest rf
      nbDigitsInFailureCount :: Int
      nbDigitsInFailureCount = floor (logBase 10 (L.genericLength failures) :: Double)
      padFailureDetails = (chunk (T.pack (replicate (nbDigitsInFailureCount + 4) ' ')) :)
   in map (padding :) $
        filter (not . null) $
          concat $
            indexed failures $ \w (ts, TDef (Timed TestRunResult {..} _) cs) ->
              concat
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
                      (fore (statusColour testRunResultStatus))
                      [ chunk $ statusCheckMark testRunResultStatus,
                        chunk $ T.pack (printf ("%" ++ show nbDigitsInFailureCount ++ "d ") w),
                        chunk $ T.intercalate "." ts
                      ]
                  ],
                  map (padFailureDetails . (: []) . chunk . T.pack) $
                    case (testRunResultNumTests, testRunResultNumShrinks) of
                      (Nothing, _) -> []
                      (Just numTests, Nothing) -> [printf "Failed after %d tests" numTests]
                      (Just numTests, Just 0) -> [printf "Failed after %d tests" numTests]
                      (Just numTests, Just numShrinks) -> [printf "Failed after %d tests and %d shrinks" numTests numShrinks],
                  map (padFailureDetails . (\c -> [chunk "Generated: ", c]) . fore yellow . chunk . T.pack) testRunResultFailingInputs,
                  map padFailureDetails $ outputFailureLabels testRunResultLabels,
                  map padFailureDetails $ outputFailureClasses testRunResultClasses,
                  map padFailureDetails $
                    case testRunResultException of
                      Nothing -> []
                      Just (Left s) -> stringChunks s
                      Just (Right a) -> outputAssertion a,
                  [padFailureDetails $ outputGoldenCase gc | gc <- maybeToList testRunResultGoldenCase],
                  concat [map padFailureDetails $ stringChunks ei | ei <- maybeToList testRunResultExtraInfo],
                  [[chunk ""]]
                ]

outputAssertion :: Assertion -> [[Chunk]]
outputAssertion = \case
  NotEqualButShouldHaveBeenEqual actual expected -> outputEqualityAssertionFailed actual expected
  EqualButShouldNotHaveBeenEqual actual notExpected -> outputNotEqualAssertionFailed actual notExpected
  PredicateFailedButShouldHaveSucceeded actual mName -> outputPredicateSuccessAssertionFailed actual mName
  PredicateSucceededButShouldHaveFailed actual mName -> outputPredicateFailAssertionFailed actual mName
  ExpectationFailed s -> stringChunks s
  Context a' context -> outputAssertion a' ++ stringChunks context

outputEqualityAssertionFailed :: String -> String -> [[Chunk]]
outputEqualityAssertionFailed actual expected =
  let diff = getDiff actual expected -- TODO use 'getGroupedDiff' instead, but then we need to fix the 'splitWhen' below
      splitLines = splitWhen ((== "\n") . chunkText)
      chunksLinesWithHeader :: Chunk -> [[Chunk]] -> [[Chunk]]
      chunksLinesWithHeader header = \case
        [cs] -> [header : cs]
        cs -> [header] : cs
      actualChunks :: [[Chunk]]
      actualChunks = chunksLinesWithHeader (fore blue "Actual:   ") $
        splitLines $
          flip mapMaybe diff $ \case
            First a -> Just $ fore red $ chunk (T.singleton a)
            Second _ -> Nothing
            Both a _ -> Just $ chunk (T.singleton a)
      expectedChunks :: [[Chunk]]
      expectedChunks = chunksLinesWithHeader (fore blue "Expected: ") $
        splitLines $
          flip mapMaybe diff $ \case
            First _ -> Nothing
            Second a -> Just $ fore green $ chunk (T.singleton a)
            Both a _ -> Just $ chunk (T.singleton a)
      inlineDiffChunks :: [[Chunk]]
      inlineDiffChunks =
        if length (lines actual) == 1 && length (lines expected) == 1
          then []
          else chunksLinesWithHeader (fore blue "Inline diff: ") $
            splitLines $
              flip map diff $ \case
                First a -> fore red $ chunk (T.singleton a)
                Second a -> fore green $ chunk (T.singleton a)
                Both a _ -> chunk (T.singleton a)
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

outputFailure :: TestRunResult -> Maybe [[Chunk]]
outputFailure TestRunResult {..} = case testRunResultStatus of
  TestPassed -> Nothing
  TestFailed -> Just [[chunk "Failure"]]

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
      DefWrapNode _ sdf -> goF level sdf
      DefBeforeAllNode _ sdf -> goF level sdf
      DefAroundAllNode _ sdf -> goF level sdf
      DefAroundAllWithNode _ sdf -> goF level sdf
      DefAfterAllNode _ sdf -> goF level sdf
      DefParallelismNode _ sdf -> goF level sdf
      DefRandomisationNode _ sdf -> goF level sdf
      DefFlakinessNode _ sdf -> goF level sdf

padding :: Chunk
padding = chunk $ T.replicate paddingSize " "

paddingSize :: Int
paddingSize = 2

orange :: Colour
orange = colour256 166

darkRed :: Colour
darkRed = colour256 160

#ifdef mingw32_HOST_OS
detectTerminalCapabilities :: IO TerminalCapabilities
detectTerminalCapabilities = do
  supports <- hSupportsANSIColor stdout
  if supports
    then pure With8BitColours
    else pure WithoutColours
#else
detectTerminalCapabilities :: IO TerminalCapabilities
detectTerminalCapabilities = getTerminalCapabilitiesFromEnv
#endif
