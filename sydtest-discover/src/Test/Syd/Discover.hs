{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Discover where

import Control.Monad.IO.Class
import Data.List
import Path
import Path.IO
import System.Environment
import System.Exit
import qualified System.FilePath as FP

sydTestDiscover :: IO ()
sydTestDiscover = do
  args <- getArgs
  print args
  case args of
    (src : _ : dest : restArgs) -> do
      here <- getCurrentDir
      specSourceFile <- resolveFile' src
      -- We asume that the spec source is a top-level module.
      let testDir = parent specSourceFile
      specSourceFileRel <- stripProperPrefix testDir specSourceFile
      otherSpecFiles <- sort . filter (\fp -> fp /= specSourceFileRel && isHaskellFile fp) <$> sourceFilesInNonHiddenDirsRecursively testDir
      let sets = Settings {settingMain = True}
      let output = makeSpecModule sets specSourceFileRel otherSpecFiles
      putStrLn output
      writeFile dest output
    _ -> die "args don't make sense"

sourceFilesInNonHiddenDirsRecursively ::
  forall m i.
  MonadIO m =>
  Path Abs Dir ->
  m [Path Rel File]
sourceFilesInNonHiddenDirsRecursively =
  walkDirAccumRel (Just goWalk) goOutput
  where
    goWalk ::
      Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (WalkAction Rel)
    goWalk curdir subdirs files = do
      pure $ WalkExclude $ filter (isHiddenIn curdir) subdirs
    goOutput ::
      Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m [Path Rel File]
    goOutput curdir subdirs files =
      pure $ map (curdir </>) $ filter (not . hidden) files

hidden :: Path Rel File -> Bool
hidden = goFile
  where
    goFile :: Path Rel File -> Bool
    goFile f = isHiddenIn (parent f) f || goDir (parent f)
    goDir :: Path Rel Dir -> Bool
    goDir f
      | parent f == f = False
      | otherwise = isHiddenIn (parent f) f || goDir (parent f)

isHiddenIn :: Path b Dir -> Path b t -> Bool
isHiddenIn curdir ad =
  case stripProperPrefix curdir ad of
    Nothing -> False
    Just rp -> "." `isPrefixOf` toFilePath rp

isHaskellFile :: Path Rel File -> Bool
isHaskellFile p = case fileExtension p of
  Just ".hs" -> True
  Just ".lhs" -> True
  _ -> False

makeModuleName :: Path Rel File -> String
makeModuleName fp =
  intercalate "." $ FP.splitDirectories $ FP.dropExtensions $ fromRelFile fp

makeSpecModule :: Settings -> Path Rel File -> [Path Rel File] -> String
makeSpecModule Settings {..} destination sources =
  unlines
    [ moduleDeclaration
        ( if settingMain then "Main" else makeModuleName destination
        ),
      "",
      "import Test.Syd (Spec, sydTest)",
      "",
      importDeclarations sources,
      if settingMain then mainDeclaration else "",
      specDeclaration sources
    ]

moduleDeclaration :: String -> String
moduleDeclaration mn = unwords ["module", mn, "where"]

mainDeclaration :: String
mainDeclaration =
  unlines
    [ "main :: IO ()",
      "main = sydTest spec"
    ]

importDeclarations :: [Path Rel File] -> String
importDeclarations = unlines . map (("import qualified " <>) . makeModuleName)

specDeclaration :: [Path Rel File] -> String
specDeclaration fs =
  unlines $
    "spec :: Spec" :
    "spec = do" :
    map (("  " ++) . specFunctionName) fs

specFunctionName :: Path Rel File -> String
specFunctionName rf = makeModuleName rf ++ ".spec"

data Settings = Settings {settingMain :: Bool} deriving (Show, Eq)
