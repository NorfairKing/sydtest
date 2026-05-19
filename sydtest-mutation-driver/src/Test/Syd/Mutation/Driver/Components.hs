{-# LANGUAGE QuasiQuotes #-}

-- | Read a Cabal file and extract the declared component names of one
-- kind ('executables' or 'test-suites').  Used by the Nix harness to
-- discover declared components at build time without having to parse the
-- .cabal file with shell tools.
--
-- The @list-components@ subcommand prints the names; the
-- @install-components@ subcommand reads them and copies the built
-- executables.
module Test.Syd.Mutation.Driver.Components
  ( readComponentNames,
    runListComponents,
    runInstallComponents,
    MissingBuiltComponent (..),
  )
where

import Control.Exception (Exception, throwIO)
import Distribution.PackageDescription
  ( GenericPackageDescription (..),
  )
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Verbosity (silent)
import Path
import Path.IO (copyFile, doesFileExist, ensureDir, getCurrentDir)
import Test.Syd.Mutation.Driver.OptParse (ComponentKind (..))

-- | Read the declared component names of one kind from a .cabal file.
--
-- Order is the order in which Cabal returns them, which is the order they
-- appear in the file.
readComponentNames :: ComponentKind -> Path Abs File -> IO [String]
readComponentNames kind cabalFile = do
  gpd <- readGenericPackageDescription silent (fromAbsFile cabalFile)
  pure $ case kind of
    ComponentExecutables -> map (unUnqualComponentName . fst) (condExecutables gpd)
    ComponentTestSuites -> map (unUnqualComponentName . fst) (condTestSuites gpd)

-- | Top-level entry point for the @list-components@ subcommand: print
-- each component name on its own line.
runListComponents :: ComponentKind -> Path Abs File -> IO ()
runListComponents kind cabalFile = do
  names <- readComponentNames kind cabalFile
  mapM_ putStrLn names

-- | Thrown by 'runInstallComponents' when a declared component does not
-- have a built executable at the expected @dist/build/<n>/<n>@ path.
-- This matches the previous shell-loop's exit-1-with-message behaviour
-- but surfaces a typed exception instead.
data MissingBuiltComponent = MissingBuiltComponent
  { missingBuiltComponentName :: !String,
    missingBuiltComponentExpectedAt :: !(Path Abs File)
  }
  deriving (Show)

instance Exception MissingBuiltComponent

-- | Top-level entry point for the @install-components@ subcommand: read
-- the declared component names from the cabal file, then for each name
-- copy @dist/build/<name>/<name>@ to @<outDir>/<name>@.  Creates
-- @outDir@ if it does not already exist.  Throws 'MissingBuiltComponent'
-- when a declared component's executable is not present where expected.
--
-- The @dist/build@ path is resolved relative to the current working
-- directory, which is how Cabal lays out its build artefacts during a
-- @Setup build@.
runInstallComponents :: ComponentKind -> Path Abs File -> Path Abs Dir -> IO ()
runInstallComponents kind cabalFile outDir = do
  names <- readComponentNames kind cabalFile
  case names of
    [] -> pure ()
    _ -> do
      ensureDir outDir
      mapM_ (installOne outDir) names
  where
    installOne :: Path Abs Dir -> String -> IO ()
    installOne destDir name = do
      relSubDir <- parseRelDir name
      relBin <- parseRelFile name
      cwd <- getCurrentDir
      let srcFile = cwd </> [reldir|dist/build|] </> relSubDir </> relBin
          destFile = destDir </> relBin
      exists <- doesFileExist srcFile
      if exists
        then copyFile srcFile destFile
        else
          throwIO
            MissingBuiltComponent
              { missingBuiltComponentName = name,
                missingBuiltComponentExpectedAt = srcFile
              }
