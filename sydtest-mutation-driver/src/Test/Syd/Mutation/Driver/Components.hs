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
    listComponentsFromCabalFile,
    runInstallComponents,
    runInstallComponentsWithBuildDir,
    findCabalFile,
    CabalFileLookupError (..),
    MissingBuiltComponent (..),
  )
where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as SB
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Distribution.PackageDescription
  ( GenericPackageDescription (..),
  )
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Verbosity (silent)
import Path
import Path.IO (copyFile, doesFileExist, ensureDir, getCurrentDir, listDirRel)
import Test.Syd.Mutation.Driver.OptParse (ComponentKind (..))

-- | Thrown by 'findCabalFile' when the lookup cannot disambiguate the
-- right cabal file.
data CabalFileLookupError
  = -- | The directory contains no @.cabal@ file at all.
    NoCabalFileFound !(Path Abs Dir)
  | -- | The directory has multiple @.cabal@ files and none of them
    -- matches the preferred @<pname>.cabal@ name.  We do not want to
    -- silently pick one (the previous shell logic did, via @head -n1@,
    -- which silently masked typos in the cabal-file basename).
    AmbiguousCabalFile
      !(Path Abs Dir)
      !String -- preferred pname (without extension)
      ![Path Rel File] -- the conflicting candidates
  deriving (Show)

instance Exception CabalFileLookupError

-- | Locate the @.cabal@ file for a package in a directory.  Prefers
-- @<pname>.cabal@; falls back to a single other @.cabal@ in the
-- directory.  Throws 'CabalFileLookupError' on no-match or ambiguous
-- match.
--
-- Mirrors the shell logic the @postInstall@ blocks used to inline:
--
-- @
-- if [ -f "<pname>.cabal" ]; then …
-- else cabalFile=$(ls -1 *.cabal 2>/dev/null | head -n1); fi
-- @
--
-- with one stricter twist: when the preferred name is not found and
-- multiple other @.cabal@ files are present, the shell silently picked
-- the first; we throw 'AmbiguousCabalFile' instead, so a typo in
-- @<pname>@ is not silently masked.
findCabalFile :: String -> Path Abs Dir -> IO (Path Abs File)
findCabalFile pname dir = do
  preferredRel <- parseRelFile (pname ++ ".cabal")
  let preferred = dir </> preferredRel
  preferredExists <- doesFileExist preferred
  if preferredExists
    then pure preferred
    else do
      (_, files) <- listDirRel dir
      let cabals = sort (filter ((== Just ".cabal") . fileExtension) files)
      case cabals of
        [] -> throwIO (NoCabalFileFound dir)
        [one] -> pure (dir </> one)
        _ -> throwIO (AmbiguousCabalFile dir pname cabals)

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

-- | Print the declared component names from a .cabal file, one per
-- line.  The function used directly by 'runListComponents' once the
-- cabal file has been resolved.
listComponentsFromCabalFile :: ComponentKind -> Path Abs File -> IO ()
listComponentsFromCabalFile kind cabalFile = do
  names <- readComponentNames kind cabalFile
  SB.putStr (TE.encodeUtf8 (T.unlines (map T.pack names)))

-- | Top-level entry point for the @list-components@ subcommand.
-- Resolves @<pname>.cabal@ in the driver's current working directory
-- via 'findCabalFile' and then delegates to 'listComponentsFromCabalFile'.
runListComponents :: ComponentKind -> String -> IO ()
runListComponents kind pname = do
  cwd <- getCurrentDir
  cabalFile <- findCabalFile pname cwd
  listComponentsFromCabalFile kind cabalFile

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

-- | Read the declared component names from the cabal file, then for
-- each name copy @<buildDir>/<name>/<name>@ to @<outDir>/<name>@.
-- Creates @outDir@ if it does not already exist.  Throws
-- 'MissingBuiltComponent' when a declared component's executable is not
-- present where expected.
--
-- The @buildDir@ is typically @<PWD>/dist/build@ during a Cabal
-- @Setup build@.  It is taken as an explicit argument (rather than
-- derived from the current working directory) so this function does not
-- mutate global process state — tests that exercise it can use
-- independent tmp dirs in parallel without fighting for CWD.
runInstallComponentsWithBuildDir ::
  ComponentKind ->
  -- | Path to the .cabal file.
  Path Abs File ->
  -- | Cabal build directory (e.g. @<pkg>/dist/build@).
  Path Abs Dir ->
  -- | Output directory to copy executables into.
  Path Abs Dir ->
  IO ()
runInstallComponentsWithBuildDir kind cabalFile buildDir outDir = do
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
      let srcFile = buildDir </> relSubDir </> relBin
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

-- | Top-level entry point for the @install-components@ subcommand.
--
-- Resolves the cabal file by looking up @<pname>.cabal@ in the
-- driver's current working directory (with the same fallback rules as
-- 'findCabalFile'), takes the Cabal build directory to be
-- @<PWD>/dist/build@ (which is how Cabal lays out its build artefacts
-- during a @Setup build@), then delegates to
-- 'runInstallComponentsWithBuildDir'.
runInstallComponents :: ComponentKind -> String -> Path Abs Dir -> IO ()
runInstallComponents kind pname outDir = do
  cwd <- getCurrentDir
  cabalFile <- findCabalFile pname cwd
  runInstallComponentsWithBuildDir
    kind
    cabalFile
    (cwd </> [reldir|dist/build|])
    outDir
