module Main where

import Distribution.PackageDescription
  ( GenericPackageDescription (..),
  )
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Verbosity (silent)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [kind, path] -> do
      gpd <- readGenericPackageDescription silent path
      names <- case kind of
        "executables" -> pure $ map (unUnqualComponentName . fst) (condExecutables gpd)
        "test-suites" -> pure $ map (unUnqualComponentName . fst) (condTestSuites gpd)
        _ -> die ("sydtest-cabal-components: unknown kind '" ++ kind ++ "'; expected 'executables' or 'test-suites'")
      mapM_ putStrLn names
    _ -> die "usage: sydtest-cabal-components <executables|test-suites> <path/to/pkg.cabal>"
