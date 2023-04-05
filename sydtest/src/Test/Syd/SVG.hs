{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.SVG (writeSvgReport) where

import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Graphics.Svg as Svg
import Test.Syd.Run
import Test.Syd.SpecDef
import Test.Syd.SpecForest

writeSvgReport :: FilePath -> Timed ResultForest -> IO ()
writeSvgReport fp trf = do
  let svgLBs = Svg.renderBS $ timedResultForestElement trf
  let completeFile =
        mconcat
          [ "<html><head><style>",
            style,
            "</style></head><body><div id=\"container\">",
            svgLBs,
            "</div></body></html>"
          ]
  LB.writeFile fp completeFile

timedResultForestElement :: Timed ResultForest -> Svg.Element
timedResultForestElement trf =
  let tests = flattenSpecForest (timedValue trf)
      runBegin = timedBegin trf
      runEnd = timedEnd trf
      maximumMay [] = Nothing
      maximumMay l = Just $ maximum l
      maxWorker :: Int
      maxWorker = fromMaybe 0 $ maximumMay $ map (timedWorker . testDefVal . snd) tests
   in ( \e ->
          with
            (svg11_ e)
            [ Height_ <<- fromString (show (workerY (maxWorker + 1)) <> "px"),
              Width_ <<- fromString (show (timeX (runEnd - runBegin + 1_000_000_000)) <> "px")
            ]
      )
        $ mconcat
          [ -- Thread labels
            g_ [] $
              mconcat $
                flip map [0 .. maxWorker] $ \workerIx ->
                  text_
                    [ X_ <<- "0",
                      Y_ <<- fromString (show (workerY workerIx))
                    ]
                    (toElement (show workerIx)),
            -- Timing labels
            g_ [] $
              mconcat $
                flip
                  map
                  [ 0,
                    1_000_000_000 -- In steps of 1 second
                    .. (runEnd - runBegin)
                  ]
                  $ \t ->
                    mconcat
                      [ -- Label
                        text_
                          [ X_ <<- fromString (show (timeX t)),
                            Y_ <<- fromString (show (topBarHeight - fontSize))
                          ]
                          (toElement (show (t `div` 1_000_000) <> " ms")),
                        -- Line
                        line_
                          [ X1_ <<- fromString (show (timeX t)),
                            Y1_ <<- fromString (show topBarHeight),
                            X2_ <<- fromString (show (timeX t)),
                            Y2_ <<- fromString (show (workerY (maxWorker + 1))),
                            Class_ <<- "time"
                          ]
                          ""
                      ],
            g_ [] $
              mconcat $
                flip map tests $ \(path, TDef timed _) ->
                  let begin = timedBegin timed - runBegin
                      end = timedEnd timed - runBegin
                      duration = end - begin
                      workerIx = timedWorker timed
                      title =
                        T.pack $
                          unlines
                            [ show $ T.intercalate "." path,
                              show (duration `div` 1_000_000) <> "ms"
                            ]
                   in mconcat
                        [ rect_
                            [ X_ <<- fromString (show (timeX begin)),
                              Y_ <<- fromString (show (workerY workerIx - barHeight `div` 2)),
                              Font_size_ <<- fromString (show fontSize),
                              Width_ <<- fromString (show (nanosToX duration)),
                              Height_ <<- fromString (show barHeight),
                              Class_ <<- ("test " <> categoriseTest duration)
                            ]
                            ( title_
                                []
                                ( text_
                                    []
                                    (toElement title)
                                )
                            )
                        ]
          ]

fontSize :: Int
fontSize = 20

timeX :: Word64 -> Int
timeX time = leftBarWidth + nanosToX time

workerY :: Int -> Int
workerY workerIx = topBarHeight + (workerIx + 1) * barHeight + workerIx * barSpacing

topBarHeight :: Int
topBarHeight = 50

leftBarWidth :: Int
leftBarWidth = 50

barHeight :: Int
barHeight = 40

barSpacing :: Int
barSpacing = 5

nanosToX :: Word64 -> Int
nanosToX n =
  fromIntegral $
    n `div` 1_000_000 -- 1 svg unit = 10 microsecond

categoriseTest :: Word64 -> Text
categoriseTest duration =
  let t = fromIntegral duration / 1_000_000 :: Double -- milliseconds
   in if
          | t < 10 -> "very-fast"
          | t < 100 -> "fast"
          | t < 1_000 -> "medium"
          | t < 10_000 -> "slow"
          | otherwise -> "very-slow"

style :: LB.ByteString
style =
  LB.intercalate
    "\n"
    [ "div#container {",
      "  height: 100%;",
      "  width: 100%;",
      "  overflow: scroll;",
      "}",
      "svg {",
      "  border: 1px dotted grey;",
      "}",
      ".test { pointer-events: all; }",
      ".very-fast {",
      "  stroke: #23ac05;",
      "  fill: #2acf06;",
      "}",
      ".fast {",
      "  stroke: #67bf16;",
      "  fill: #78df1a;",
      "}",
      ".medium {",
      "  stroke: #c4d600;",
      "  fill: #e5f900;",
      "}",
      ".slow {",
      "  stroke: #b19601;",
      "  fill: #d4b401;",
      "}",
      ".very-slow {",
      "  fill: #d60000;",
      "  stroke: #b20000;",
      "}",
      ".test {",
      "}",
      ".test:hover {",
      "  stroke: cyan !important;",
      "}",
      ".time {",
      "  stroke: black;",
      "  stroke-width: 1;",
      "  stroke-dasharray: 10,10;",
      "  opacity: 0.5",
      "}"
    ]
