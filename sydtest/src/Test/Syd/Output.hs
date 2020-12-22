{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Syd.Output where

import Control.Monad.Reader
import Data.Algorithm.Diff
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.List
import Data.List.Split (splitWhen)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack
import Rainbow
import Rainbow.Types (Chunk (..))
import Safe
import Test.QuickCheck.IO ()
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest
import Text.Printf

printOutputSpecForest :: Maybe Bool -> Timed ResultForest -> IO ()
printOutputSpecForest mColour results = do
  byteStringMaker <- case mColour of
    Just False -> pure toByteStringsColors0
    Just True -> pure toByteStringsColors256
    Nothing -> liftIO byteStringMakerFromEnvironment
  let bytestrings = outputSpecForestByteString byteStringMaker results
  forM_ bytestrings $ \bs -> do
    mapM_ SB.putStr bs
    SB8.putStrLn ""

outputSpecForestByteString :: (Chunk -> [ByteString] -> [ByteString]) -> Timed ResultForest -> [[ByteString]]
outputSpecForestByteString byteStringMaker results = map (chunksToByteStrings byteStringMaker) (outputResultReport results)

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
  let totalTimeSeconds :: Double
      totalTimeSeconds = fromIntegral timing / 1_000_000_000
   in map (padding :) $
        concat
          [ [ [ chunk "Passed:                   ",
                fore green $ chunk (T.pack (show testSuiteStatSuccesses))
              ],
              [ chunk "Failed:                   ",
                ( if testSuiteStatFailures > 0
                    then fore red
                    else fore green
                )
                  $ chunk (T.pack (show testSuiteStatFailures))
              ]
            ],
            [ [ chunk "Pending:                  ",
                fore magenta $ chunk (T.pack (show testSuiteStatPending))
              ]
              | testSuiteStatPending > 0
            ],
            [ let longestTimeSeconds :: Double
                  longestTimeSeconds = fromIntegral longest / 1_000_000_000
                  longestTimePercentage :: Double
                  longestTimePercentage = 100 * longestTimeSeconds / totalTimeSeconds
               in concat
                    [ [ chunk "Longest test took",
                        fore yellow $ chunk $ T.pack (printf "%13.2f seconds" longestTimeSeconds)
                      ],
                      [ chunk $ T.pack (printf ", which is %.2f%% of total runtime" longestTimePercentage)
                        | longestTimePercentage > 50
                      ]
                    ]
              | longest <- maybeToList testSuiteStatLongestTime
            ],
            [ [ chunk "Test suite took  ",
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
            [ pad
                [chunk (T.pack (printf "  (passed for all of %d inputs)" w))]
              | testRunResultStatus == TestPassed,
                w <- maybeToList testRunResultNumTests
            ],
            [pad $ outputGoldenCase gc | gc <- maybeToList testRunResultGoldenCase]
          ]

outputPendingLines :: Text -> Maybe Text -> [[Chunk]]
outputPendingLines specifyText mReason =
  filter
    (not . null)
    [ [fore magenta $ chunk specifyText],
      case mReason of
        Nothing -> []
        Just reason -> [padding, chunk reason]
    ]

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
      nbDigitsInFailureCount = floor (logBase 10 (genericLength failures) :: Double)
      pad = (chunk (T.pack (replicate (nbDigitsInFailureCount + 4) ' ')) :)
      padFailureDetails = (chunk (T.pack (replicate (1 + nbDigitsInFailureCount + 3) ' ')) :)
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
                  map (pad . (: []) . chunk . T.pack) $
                    case (testRunResultNumTests, testRunResultNumShrinks) of
                      (Nothing, _) -> []
                      (Just numTests, Nothing) -> [printf "Failled after %d tests" numTests]
                      (Just numTests, Just 0) -> [printf "Failled after %d tests" numTests]
                      (Just numTests, Just numShrinks) -> [printf "Failed after %d tests and %d shrinks" numTests numShrinks],
                  map (pad . (\c -> [chunk "Generated: ", c]) . fore yellow . chunk . T.pack) testRunResultFailingInputs,
                  map pad $
                    case testRunResultException of
                      Nothing -> []
                      Just (Left s) -> stringChunks s
                      Just (Right a) -> case a of
                        NotEqualButShouldHaveBeenEqual actual expected mContext -> outputEqualityAssertionFailed actual expected mContext
                        EqualButShouldNotHaveBeenEqual actual notExpected mContext -> outputNotEqualAssertionFailed actual notExpected mContext
                        PredicateFailedButShouldHaveSucceeded actual mContext -> outputPredicateSuccessAssertionFailed actual mContext
                        PredicateSucceededButShouldHaveFailed actual mContext -> outputPredicateFailAssertionFailed actual mContext
                        ExpectationFailed s -> stringChunks s,
                  [padFailureDetails $ outputGoldenCase gc | gc <- maybeToList testRunResultGoldenCase],
                  [[chunk ""]]
                ]

outputEqualityAssertionFailed :: String -> String -> Maybe String -> [[Chunk]]
outputEqualityAssertionFailed actual expected mContext =
  let diff = getDiff actual expected -- TODO use 'getGroupedDiff' instead, but then we need to fix the 'splitWhen' below
      splitLines = splitWhen ((== "\n") . _yarn)
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
          inlineDiffChunks,
          mContextChunks mContext
        ]

outputNotEqualAssertionFailed :: String -> String -> Maybe String -> [[Chunk]]
outputNotEqualAssertionFailed actual notExpected mContext =
  ( if actual == notExpected -- String equality
      then
        [ [chunk "Did not expect equality of the values but both were:"],
          [chunk (T.pack actual)]
        ]
      else
        [ [chunk "These two values were considered equal but should not have been equal:"],
          [fore blue "Actual      : ", chunk (T.pack actual)],
          [fore blue "Not Expected: ", chunk (T.pack notExpected)]
        ]
  )
    ++ mContextChunks mContext

outputPredicateSuccessAssertionFailed :: String -> Maybe String -> [[Chunk]]
outputPredicateSuccessAssertionFailed actual mContext =
  [ [chunk "Predicate failed, but should have succeeded, on this value:"],
    [chunk (T.pack actual)]
  ]
    ++ mContextChunks mContext

outputPredicateFailAssertionFailed :: String -> Maybe String -> [[Chunk]]
outputPredicateFailAssertionFailed actual mContext =
  [ [chunk "Predicate succeeded, but should have failed, on this value:"],
    [chunk (T.pack actual)]
  ]
    ++ mContextChunks mContext

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

statusColour :: TestStatus -> Radiant
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

padding :: Chunk
padding = chunk $ T.replicate paddingSize " "

paddingSize :: Int
paddingSize = 2

orange :: Radiant
orange = color256 166

darkRed :: Radiant
darkRed = color256 160
