{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Test.Syd.Def
import Test.Syd.Run
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
      outputSpecForest rf,
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

outputSpecForest :: ResultForest -> [[Chunk]]
outputSpecForest = concatMap outputSpecTree

outputSpecTree :: ResultTree -> [[Chunk]]
outputSpecTree = \case
  DescribeNode t sf -> outputDescribeLine t : map (chunk "  " :) (outputSpecForest sf)
  SpecifyNode t td -> outputSpecifyLines t td
  AroundAllNode sf -> outputSpecForest sf

outputDescribeLine :: Text -> [Chunk]
outputDescribeLine t = [fore yellow $ chunk t]

outputSpecifyLines :: Text -> TestDef TestRunResult -> [[Chunk]]
outputSpecifyLines t (TestDef (TestRunResult {..}) _) =
  map (map (fore (statusColour testRunResultStatus))) $
    filter
      (not . null)
      [ [ chunk (statusCheckMark testRunResultStatus),
          chunk t
        ],
        concat
          [ -- [chunk (T.pack (printf "%10.2f ms " (testRunResultExecutionTime * 1000)))],
            [chunk (T.pack (printf "  (passed for all of %d inputs)" w)) | w <- maybeToList testRunResultNumTests, testRunResultStatus == TestPassed]
          ]
      ]

testFailed :: (a, TestDef TestRunResult) -> Bool
testFailed = (== TestFailed) . testRunResultStatus . testDefVal . snd

outputFailures :: ResultForest -> [[Chunk]]
outputFailures rf =
  let failures = filter testFailed $ flattenSpecForest rf
      nbDigitsInFailureCount :: Int
      nbDigitsInFailureCount = ceiling (logBase 10 (genericLength failures) :: Double)
      pad = (chunk (T.replicate (nbDigitsInFailureCount + 3) " ") :)
   in map (chunk "  " :) $
        filter (not . null) $
          concat $
            indexed failures $ \w (ts, TestDef (TestRunResult {..}) cs) ->
              concat
                [ [ [ (fore cyan) $
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
        flip mapMaybe diff $ \d -> case d of
          Both a _ -> Just $ chunk (T.singleton a)
          First a -> Just $ fore red $ chunk (T.singleton a)
          _ -> Nothing
      expectedChunks = splitLines $
        flip mapMaybe diff $ \d -> case d of
          Both a _ -> Just $ chunk (T.singleton a)
          Second a -> Just $ fore green $ chunk (T.singleton a)
          _ -> Nothing
      chunksLinesWithHeader :: Chunk -> [[Chunk]] -> [[Chunk]]
      chunksLinesWithHeader header = \case
        [cs] -> [header : cs]
        cs -> [header] : cs
   in concat
        [ [[chunk "Expected these values to be equal:"]],
          chunksLinesWithHeader (fore blue $ "Actual:   ") actualChunks,
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
        [fore blue $ "Actual      : ", chunk (T.pack actual)],
        [fore blue $ "Not Expected: ", chunk (T.pack notExpected)]
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