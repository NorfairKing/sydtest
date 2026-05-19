-- | Read a Cabal file and extract the declared component names of one
-- kind ('executables' or 'test-suites').  Used by the Nix harness to
-- discover declared components at build time without having to parse the
-- .cabal file with shell tools.
--
-- This module is parser-only.  The @list-components@ subcommand prints
-- the names; the @install-components@ subcommand reads them and copies
-- the built executables.
module Test.Syd.Mutation.Driver.Components
  ( readComponentNames,
    runListComponents,
  )
where

import Distribution.PackageDescription
  ( GenericPackageDescription (..),
  )
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Verbosity (silent)
import Path
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
