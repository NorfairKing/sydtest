{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.ManifestRenderSpec (spec) where

import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as TE
import Path
import Path.IO (withSystemTempDir)
import Test.Syd
import Test.Syd.Mutation.Manifest.Render (renderManifest, writeManifestTxtFile)
import Test.Syd.Validity
import Text.Colour

spec :: Spec
spec =
  describe "writeManifestTxtFile" $
    it "writes exactly the rendered manifest, encoded as UTF-8" $
      forAllValid $ \manifest ->
        withSystemTempDir "sydtest-mutation-manifest-txt" $ \dir -> do
          writeManifestTxtFile dir "Example.Mod" manifest
          contents <- SB.readFile (fromAbsFile (dir </> [relfile|Example.Mod.txt|]))
          TE.decodeUtf8' contents
            `shouldBe` Right (renderChunksText With8BitColours (unlinesChunks (renderManifest "Example.Mod" manifest)))
