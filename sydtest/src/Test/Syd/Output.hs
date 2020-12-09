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

printOutputSpecForest :: ResultForest -> IO ()
printOutputSpecForest results = do
  byteStringMaker <- byteStringMakerFromEnvironment
  let bytestrings = map (chunksToByteStrings byteStringMaker) (outputResultReport results) :: [[ByteString]]
  forM_ bytestrings $ \bs -> do
    mapM_ SB.putStr bs
    SB8.putStrLn ""

outputResultReport :: ResultForest -> [[Chunk]]
outputResultReport rf =
  concat
    [ outputTestsHeader,
      outputSpecForest 0 (resultForestWidth rf) rf,
      [ [chunk ""],
        [chunk ""]
      ],
      outputFailuresWithHeading rf
    ]

outputFailuresWithHeading :: ResultForest -> [[Chunk]]
outputFailuresWithHeading rf =
  if any testFailed (flattenSpecForest rf)
    then
      concat
        [ outputFailuresHeader,
          outputFailures rf
        ]
    else []

outputTestsHeader :: [[Chunk]]
outputTestsHeader = outputHeader "Tests:"

outputFailuresHeader :: [[Chunk]]
outputFailuresHeader = outputHeader "Failures:"

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
  PendingNode t -> [outputPendingLine t]
  DescribeNode t sf -> outputDescribeLine t : map (padding :) (outputSpecForest (level + 1) treeWidth sf)
  SubForestNode sf -> outputSpecForest level treeWidth sf

outputDescribeLine :: Text -> [Chunk]
outputDescribeLine t = [fore yellow $ chunk t]

outputSpecifyLines :: Int -> Int -> Text -> TestDef (Timed TestRunResult) -> [[Chunk]]
outputSpecifyLines level treeWidth specifyText (TestDef (Timed TestRunResult {..} executionTime) _) =
  let t = fromIntegral executionTime / 1_000_000 :: Double -- milliseconds
      executionTimeText = T.pack (printf "%10.2f ms" t)
      withTimingColour =
        if
            | t < 10 -> fore green
            | t < 1000 && t >= 10 -> fore yellow
            | otherwise -> fore red

      withStatusColour = fore (statusColour testRunResultStatus)
   in filter
        (not . null)
        [ [ withStatusColour $ chunk (statusCheckMark testRunResultStatus),
            withStatusColour $ chunk specifyText,
            spacingChunk level specifyText executionTimeText treeWidth,
            withTimingColour $ chunk executionTimeText
          ],
          [ chunk (T.pack (printf "  (passed for all of %d inputs)" w))
            | testRunResultStatus == TestPassed,
              w <- maybeToList testRunResultNumTests
          ]
        ]

outputPendingLine :: Text -> [Chunk]
outputPendingLine specifyText = [fore magenta $ chunk specifyText]

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

testFailed :: (a, TestDef (Timed TestRunResult)) -> Bool
testFailed = (== TestFailed) . testRunResultStatus . timedValue . testDefVal . snd

outputFailures :: ResultForest -> [[Chunk]]
outputFailures rf =
  let failures = filter testFailed $ flattenSpecForest rf
      nbDigitsInFailureCount :: Int
      nbDigitsInFailureCount = ceiling (logBase 10 (genericLength failures) :: Double)
      pad = (chunk (T.replicate (nbDigitsInFailureCount + 3) " ") :)
   in map (padding :) $
        filter (not . null) $
          concat $
            indexed failures $ \w (ts, TestDef (Timed TestRunResult {..} _) cs) ->
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
                  map pad $ case testRunResultException of
                    Nothing -> []
                    Just (Left s) ->
                      let ls = lines s
                       in map ((: []) . chunk . T.pack) ls
                    Just (Right a) -> case a of
                      NotEqualButShouldHaveBeenEqual actual expected -> outputEqualityAssertionFailed actual expected
                      EqualButShouldNotHaveBeenEqual actual notExpected -> outputNotEqualAssertionFailed actual notExpected
                      PredicateFailedButShouldHaveSucceeded actual -> outputPredicateSuccessAssertionFailed actual
                      PredicateSucceededButShouldHaveFailed actual -> outputPredicateFailAssertionFailed actual,
                  [[chunk ""]]
                ]

outputEqualityAssertionFailed :: String -> String -> [[Chunk]]
outputEqualityAssertionFailed actual expected =
  let diff = getDiff actual expected
      splitLines = splitWhen ((== "\n") . _yarn)
      actualChunks = splitLines $
        flip mapMaybe diff $ \case
          Both a _ -> Just $ chunk (T.singleton a)
          First a -> Just $ fore red $ chunk (T.singleton a)
          _ -> Nothing
      expectedChunks = splitLines $
        flip mapMaybe diff $ \case
          Both a _ -> Just $ chunk (T.singleton a)
          Second a -> Just $ fore green $ chunk (T.singleton a)
          _ -> Nothing
      chunksLinesWithHeader :: Chunk -> [[Chunk]] -> [[Chunk]]
      chunksLinesWithHeader header = \case
        [cs] -> [header : cs]
        cs -> [header] : cs
   in concat
        [ [[chunk "Expected these values to be equal:"]],
          chunksLinesWithHeader (fore blue "Actual:   ") actualChunks,
          chunksLinesWithHeader (fore blue "Expected: ") expectedChunks
        ]

outputNotEqualAssertionFailed :: String -> String -> [[Chunk]]
outputNotEqualAssertionFailed actual notExpected =
  if actual == notExpected -- String equality
    then
      [ [chunk "Did not expect equality of this value:"],
        [chunk (T.pack actual)]
      ]
    else
      [ [chunk "These two values were considered equal but should not have been equal:"],
        [fore blue "Actual      : ", chunk (T.pack actual)],
        [fore blue "Not Expected: ", chunk (T.pack notExpected)]
      ]

outputPredicateSuccessAssertionFailed :: String -> [[Chunk]]
outputPredicateSuccessAssertionFailed actual =
  [ [chunk "Predicate failed, but should have succeeded, on this value:"],
    [chunk (T.pack actual)]
  ]

outputPredicateFailAssertionFailed :: String -> [[Chunk]]
outputPredicateFailAssertionFailed actual =
  [ [chunk "Predicate succeeded, but should have failed, on this value:"],
    [chunk (T.pack actual)]
  ]

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
      PendingNode t -> T.length t + level * paddingSize
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
      DefPendingNode t -> T.length t + level * paddingSize
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
