module Test.Syd.Mutation.Plugin.Operators.TH (collectOperators) where

import Control.Monad (forM)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory (listDirectory)
import System.FilePath (dropExtension, takeFileName)

-- | Scan the @Operator/@ sibling directory at splice time and produce a list
-- expression of all @theOperator@ values found there.
--
-- Each operator module must be imported (qualified) in the calling module so
-- GHC can resolve the names.  The TH splice builds the list automatically so
-- that only the import line needs to be added when a new operator is created —
-- the @allOperators@ value updates itself.
collectOperators :: Q Exp
collectOperators = do
  loc <- location
  let thisFile = loc_filename loc
      -- .../Plugin/Operators/TH.hs -> .../Plugin/Operator/
      pluginDir = reverse $ dropWhile (/= '/') $ reverse $ reverse $ dropWhile (/= '/') $ reverse thisFile
      operatorDir = pluginDir ++ "Operator"
  files <- runIO $ do
    entries <- listDirectory operatorDir
    pure $ sort $ filter (\f -> not ("." `isPrefixOf` f) && ".hs" `isSuffixOf` f) entries
  mapM_ (addDependentFile . (\f -> operatorDir ++ "/" ++ f)) files
  operatorExprs <- forM files $ \file -> do
    let modName = "Test.Syd.Mutation.Plugin.Operator." ++ dropExtension (takeFileName file)
    varE (mkName (modName ++ ".theOperator"))
  listE (map pure operatorExprs)
