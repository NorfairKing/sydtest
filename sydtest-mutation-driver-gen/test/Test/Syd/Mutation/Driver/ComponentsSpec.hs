{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Syd.Mutation.Driver.ComponentsSpec (spec) where

import qualified Control.Exception as Exception
import qualified Data.ByteString as SB
import Path
import Path.IO (createDirIfMissing, doesFileExist, withSystemTempDir)
import Test.Syd
import Test.Syd.Mutation.Driver.Components
  ( CabalFileLookupError (..),
    MissingBuiltComponent (..),
    findCabalFile,
    readComponentNames,
    runInstallComponentsWithBuildDir,
  )
import Test.Syd.Mutation.Driver.OptParse (ComponentKind (..))

spec :: Spec
spec = do
  describe "readComponentNames" $ do
    it "returns executable names in declaration order" $
      withSystemTempDir "components-exes" $ \dir -> do
        let cabalFile = dir </> [relfile|pkg.cabal|]
        SB.writeFile (fromAbsFile cabalFile) $
          mconcat
            [ "cabal-version: 2.0\n",
              "name: pkg\n",
              "version: 0.1.0.0\n",
              "build-type: Simple\n",
              "executable first-exe\n",
              "  main-is: Main.hs\n",
              "  build-depends: base\n",
              "executable second-exe\n",
              "  main-is: Main.hs\n",
              "  build-depends: base\n"
            ]
        names <- readComponentNames ComponentExecutables cabalFile
        names `shouldBe` ["first-exe", "second-exe"]

    it "returns test-suite names" $
      withSystemTempDir "components-tests" $ \dir -> do
        let cabalFile = dir </> [relfile|pkg.cabal|]
        SB.writeFile (fromAbsFile cabalFile) $
          mconcat
            [ "cabal-version: 2.0\n",
              "name: pkg\n",
              "version: 0.1.0.0\n",
              "build-type: Simple\n",
              "test-suite the-test\n",
              "  type: exitcode-stdio-1.0\n",
              "  main-is: Spec.hs\n",
              "  build-depends: base\n"
            ]
        names <- readComponentNames ComponentTestSuites cabalFile
        names `shouldBe` ["the-test"]

    it "returns an empty list when the package declares no components of that kind" $
      withSystemTempDir "components-empty" $ \dir -> do
        let cabalFile = dir </> [relfile|pkg.cabal|]
        SB.writeFile (fromAbsFile cabalFile) $
          mconcat
            [ "cabal-version: 2.0\n",
              "name: pkg\n",
              "version: 0.1.0.0\n",
              "build-type: Simple\n",
              "library\n",
              "  build-depends: base\n"
            ]
        execNames <- readComponentNames ComponentExecutables cabalFile
        testNames <- readComponentNames ComponentTestSuites cabalFile
        execNames `shouldBe` []
        testNames `shouldBe` []

  describe "runInstallComponentsWithBuildDir" $ do
    it "copies each declared executable from <buildDir>/<n>/<n> to <outDir>/<n>" $
      withSystemTempDir "install-ok" $ \dir -> do
        let cabalFile = dir </> [relfile|pkg.cabal|]
            buildDir = dir </> [reldir|dist/build|]
            outDir = dir </> [reldir|out|]
        SB.writeFile (fromAbsFile cabalFile) $
          mconcat
            [ "cabal-version: 2.0\n",
              "name: pkg\n",
              "version: 0.1.0.0\n",
              "build-type: Simple\n",
              "executable foo\n",
              "  main-is: Main.hs\n",
              "  build-depends: base\n",
              "executable bar\n",
              "  main-is: Main.hs\n",
              "  build-depends: base\n"
            ]
        createDirIfMissing True (buildDir </> [reldir|foo|])
        createDirIfMissing True (buildDir </> [reldir|bar|])
        SB.writeFile (fromAbsFile (buildDir </> [relfile|foo/foo|])) "FOO"
        SB.writeFile (fromAbsFile (buildDir </> [relfile|bar/bar|])) "BAR"
        runInstallComponentsWithBuildDir ComponentExecutables cabalFile buildDir outDir
        copiedFoo <- doesFileExist (outDir </> [relfile|foo|])
        copiedBar <- doesFileExist (outDir </> [relfile|bar|])
        copiedFoo `shouldBe` True
        copiedBar `shouldBe` True
        fooBytes <- SB.readFile (fromAbsFile (outDir </> [relfile|foo|]))
        fooBytes `shouldBe` "FOO"

    it "throws MissingBuiltComponent when a declared executable's binary is absent" $
      withSystemTempDir "install-missing" $ \dir -> do
        let cabalFile = dir </> [relfile|pkg.cabal|]
            buildDir = dir </> [reldir|dist/build|]
            outDir = dir </> [reldir|out|]
        SB.writeFile (fromAbsFile cabalFile) $
          mconcat
            [ "cabal-version: 2.0\n",
              "name: pkg\n",
              "version: 0.1.0.0\n",
              "build-type: Simple\n",
              "executable foo\n",
              "  main-is: Main.hs\n",
              "  build-depends: base\n"
            ]
        result <-
          Exception.try $
            runInstallComponentsWithBuildDir ComponentExecutables cabalFile buildDir outDir
        case result of
          Left (MissingBuiltComponent name _) -> name `shouldBe` "foo"
          Right () -> expectationFailure "expected MissingBuiltComponent to be thrown"

    it "creates the output directory when it does not exist" $
      withSystemTempDir "install-mkdir" $ \dir -> do
        let cabalFile = dir </> [relfile|pkg.cabal|]
            buildDir = dir </> [reldir|dist/build|]
            outDir = dir </> [reldir|nested/out|]
        SB.writeFile (fromAbsFile cabalFile) $
          mconcat
            [ "cabal-version: 2.0\n",
              "name: pkg\n",
              "version: 0.1.0.0\n",
              "build-type: Simple\n",
              "executable foo\n",
              "  main-is: Main.hs\n",
              "  build-depends: base\n"
            ]
        createDirIfMissing True (buildDir </> [reldir|foo|])
        SB.writeFile (fromAbsFile (buildDir </> [relfile|foo/foo|])) "FOO"
        runInstallComponentsWithBuildDir ComponentExecutables cabalFile buildDir outDir
        copied <- doesFileExist (outDir </> [relfile|foo|])
        copied `shouldBe` True

    it "is a no-op when the package declares no components of that kind" $
      withSystemTempDir "install-empty" $ \dir -> do
        let cabalFile = dir </> [relfile|pkg.cabal|]
            buildDir = dir </> [reldir|dist/build|]
            outDir = dir </> [reldir|out|]
        SB.writeFile (fromAbsFile cabalFile) $
          mconcat
            [ "cabal-version: 2.0\n",
              "name: pkg\n",
              "version: 0.1.0.0\n",
              "build-type: Simple\n",
              "library\n",
              "  build-depends: base\n"
            ]
        -- Should not throw and should not need <buildDir> to exist.
        runInstallComponentsWithBuildDir ComponentExecutables cabalFile buildDir outDir

  describe "findCabalFile" $ do
    it "returns <pname>.cabal when it exists" $
      withSystemTempDir "find-preferred" $ \dir -> do
        let expected = dir </> [relfile|foo.cabal|]
        SB.writeFile (fromAbsFile expected) "cabal-version: 2.0\nname: foo\n"
        actual <- findCabalFile "foo" dir
        actual `shouldBe` expected

    it "prefers <pname>.cabal over a differently-named .cabal also in the directory" $
      withSystemTempDir "find-prefer-over-other" $ \dir -> do
        let expected = dir </> [relfile|foo.cabal|]
        SB.writeFile (fromAbsFile expected) "cabal-version: 2.0\nname: foo\n"
        SB.writeFile (fromAbsFile (dir </> [relfile|other.cabal|])) "cabal-version: 2.0\nname: other\n"
        actual <- findCabalFile "foo" dir
        actual `shouldBe` expected

    it "falls back to the single other .cabal file when <pname>.cabal is absent" $
      withSystemTempDir "find-fallback" $ \dir -> do
        let expected = dir </> [relfile|differently-named.cabal|]
        SB.writeFile (fromAbsFile expected) "cabal-version: 2.0\nname: x\n"
        actual <- findCabalFile "foo" dir
        actual `shouldBe` expected

    it "throws NoCabalFileFound when the directory has no .cabal files" $
      withSystemTempDir "find-empty" $ \dir -> do
        result <-
          (Exception.try :: IO (Path Abs File) -> IO (Either CabalFileLookupError (Path Abs File))) $
            findCabalFile "foo" dir
        case result of
          Left (NoCabalFileFound d) -> d `shouldBe` dir
          Left e -> expectationFailure ("wrong error: " ++ show e)
          Right p -> expectationFailure ("expected NoCabalFileFound, got " ++ show p)

    it "throws AmbiguousCabalFile when <pname>.cabal is absent and multiple other .cabal files match" $
      withSystemTempDir "find-ambiguous" $ \dir -> do
        SB.writeFile (fromAbsFile (dir </> [relfile|a.cabal|])) "cabal-version: 2.0\nname: a\n"
        SB.writeFile (fromAbsFile (dir </> [relfile|b.cabal|])) "cabal-version: 2.0\nname: b\n"
        result <-
          (Exception.try :: IO (Path Abs File) -> IO (Either CabalFileLookupError (Path Abs File))) $
            findCabalFile "foo" dir
        case result of
          Left (AmbiguousCabalFile d pname _) -> do
            d `shouldBe` dir
            pname `shouldBe` "foo"
          Left e -> expectationFailure ("wrong error: " ++ show e)
          Right p -> expectationFailure ("expected AmbiguousCabalFile, got " ++ show p)
