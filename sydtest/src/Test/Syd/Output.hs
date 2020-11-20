{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Syd.Output where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Stack
import Rainbow
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
  concat
    [ outputFailuresHeader,
      outputFailures rf
    ]

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

outputFailures :: ResultForest -> [[Chunk]]
outputFailures rf =
  let failures = filter ((== TestFailed) . testRunResultStatus . testDefVal . snd) $ flattenSpecForest rf
      nbDigitsInFailureCount :: Int
      nbDigitsInFailureCount = ceiling (logBase 10 (genericLength failures) :: Double)
      pad = (chunk (T.replicate (nbDigitsInFailureCount + 3) " ") :)
   in map (chunk "  " :) $ filter (not . null) $ concat $ indexed failures $ \w (ts, TestDef (TestRunResult {..}) cs) ->
        concat
          [ [ [ (fore cyan) $ chunk $ T.pack $
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
                (Just numTests, Just numShrinks) -> [printf "Failed after %d tests and %d shrinks" numTests numShrinks],
            map pad $ case testRunResultException of
              Nothing -> []
              Just (Left s) ->
                let ls = lines s
                 in map ((: []) . chunk . T.pack) ls
              Just (Right a) -> case a of
                Equality actual expected ->
                  [ [chunk "Expected these values to be equal: "],
                    [chunk "Actual:   ", chunk (T.pack actual)],
                    [chunk "Expected: ", chunk (T.pack expected)]
                  ],
            [[chunk ""]]
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
