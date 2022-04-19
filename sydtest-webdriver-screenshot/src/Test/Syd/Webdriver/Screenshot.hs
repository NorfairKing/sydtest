{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Because of webdriver using dangerous constructors
{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}
-- For the undefined trick
{-# OPTIONS_GHC -fno-warn-unused-pattern-binds #-}

module Test.Syd.Webdriver.Screenshot where

import Codec.Picture as Picture
import Control.Monad.Reader
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Path
import Path.IO
import System.Exit
import Test.Syd
import Test.Syd.Webdriver
import Test.WebDriver as WD

-- | A screenshot with location
data Screenshot = Screenshot
  { -- | File location for comparisons
    screenshotFile :: !(Path Abs File),
    -- | Decoded image
    screenshotImage :: !(Picture.Image PixelRGB8)
  }

-- | Take a screenshot and turn it into a golden test.
goldenScreenshotHere :: FilePath -> WebdriverTestM app (GoldenTest Screenshot)
goldenScreenshotHere fp = pureGoldenScreenshot fp <$> WD.screenshot

-- | Make a golden test for a given screenshot in lazy 'LB.ByteString' form.
pureGoldenScreenshot :: FilePath -> LB.ByteString -> GoldenTest Screenshot
pureGoldenScreenshot fp contents =
  GoldenTest
    { goldenTestRead = do
        relFile <- parseRelFile fp
        currentDir <- getCurrentDir
        let resolvedFile = currentDir </> relFile
        mContents <- forgivingAbsence $ SB.readFile $ fromAbsFile resolvedFile
        forM mContents $ \cts -> do
          case decodePng cts of
            Left err -> die err
            Right dynamicImage ->
              pure $
                Screenshot
                  { screenshotFile = resolvedFile,
                    screenshotImage = convertRGB8 dynamicImage
                  },
      goldenTestProduce = do
        let sb = LB.toStrict contents
        case decodePng sb of
          Left err -> expectationFailure $ "Could not parse screenshot as png: " <> err
          Right dynamicImage -> do
            let image = convertRGB8 dynamicImage
            relFile <- parseRelFile fp
            tempDir <- resolveDir' "screenshot-comparison"
            let tempFile = tempDir </> relFile
            ensureDir $ parent tempFile
            -- Write it to a file so we can compare it if it differs.
            writePng (fromAbsFile tempFile) image
            pure $
              Screenshot
                { screenshotFile = tempFile,
                  screenshotImage = image
                },
      goldenTestWrite = \(Screenshot _ actual) -> do
        relFile <- parseRelFile fp
        currentDir <- getCurrentDir
        let resolvedFile = currentDir </> relFile
        ensureDir $ parent resolvedFile
        writePng (fromAbsFile resolvedFile) actual,
      goldenTestCompare = \(Screenshot actualPath actual) (Screenshot expectedPath expected) ->
        if actual == expected
          then Nothing
          else
            Just $
              ExpectationFailed $
                unlines
                  [ "Screenshots differ.",
                    "expected: " <> fromAbsFile expectedPath,
                    "actual: " <> fromAbsFile actualPath
                  ]
    }
