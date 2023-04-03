{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.SVG (writeSvgReport) where

import qualified Data.ByteString.Lazy as LB
import Data.Maybe
import Data.String
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
                      workerIx = timedWorker timed
                   in mconcat
                        [ rect_
                            [ X_ <<- fromString (show (timeX begin)),
                              Y_ <<- fromString (show (workerY workerIx - barHeight `div` 2)),
                              Font_size_ <<- fromString (show fontSize),
                              Width_ <<- fromString (show (nanosToX (end - begin))),
                              Height_ <<- fromString (show barHeight),
                              Class_ <<- "test"
                            ]
                            ( title_
                                []
                                ( text_
                                    []
                                    (toElement (T.intercalate "." path))
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
    n `div` 10_000_000 -- 1 svg unit = 10 microsecond

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
      ".test {",
      "  fill: forestgreen;",
      "  stroke: green;",
      "  stroke-width: 2;",
      "  opacity: 0.5",
      "}",
      ".test:hover {",
      "  fill: green !important;",
      "  stroke: red !important;",
      "}",
      ".time {",
      "  stroke: black;",
      "  stroke-width: 1;",
      "  stroke-dasharray: 10,10;",
      "  opacity: 0.5",
      "}"
    ]
