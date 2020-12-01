{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Syd.Discover where

import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Options.Applicative
import Path
import Path.IO
import qualified System.FilePath as FP

sydTestDiscover :: IO ()
sydTestDiscover = do
  Arguments {..} <- getArguments
  specSourceFile <- resolveFile' argSource
  -- We asume that the spec source is a top-level module.
  let testDir = parent specSourceFile
  specSourceFileRel <- stripProperPrefix testDir specSourceFile
  otherSpecFiles <- mapMaybe parseSpecModule . sort . filter (\fp -> fp /= specSourceFileRel && isHaskellFile fp) <$> sourceFilesInNonHiddenDirsRecursively testDir
  let output = makeSpecModule argSettings specSourceFileRel otherSpecFiles
  writeFile argDestination output

data Arguments = Arguments
  { argSource :: FilePath,
    argIgnored :: FilePath,
    argDestination :: FilePath,
    argSettings :: Settings
  }
  deriving (Show, Eq)

data Settings = Settings
  { settingMain :: Bool
  }
  deriving (Show, Eq)

getArguments :: IO Arguments
getArguments = execParser $ info argumentsParser fullDesc

argumentsParser :: Parser Arguments
argumentsParser =
  Arguments
    <$> strArgument (mconcat [help "Source file path"])
    <*> strArgument (mconcat [help "Ignored argument"])
    <*> strArgument (mconcat [help "Destiantion file path"])
    <*> ( Settings
            <$> ( flag' True (mconcat [long "main", help "generate a main module and function"])
                    <|> flag' False (mconcat [long "no-main", help "don't generate a main module and function"])
                    <|> pure True
                )
        )

sourceFilesInNonHiddenDirsRecursively ::
  forall m.
  MonadIO m =>
  Path Abs Dir ->
  m [Path Rel File]
sourceFilesInNonHiddenDirsRecursively =
  walkDirAccumRel (Just goWalk) goOutput
  where
    goWalk ::
      Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m (WalkAction Rel)
    goWalk curdir subdirs _ = do
      pure $ WalkExclude $ filter (isHiddenIn curdir) subdirs
    goOutput ::
      Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> m [Path Rel File]
    goOutput curdir _ files =
      pure $ map (curdir </>) $ filter (not . hiddenFile) files

hiddenFile :: Path Rel File -> Bool
hiddenFile = goFile
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

data SpecModule = SpecModule
  { specModulePath :: Path Rel File,
    specModuleModuleName :: String,
    specModuleDescription :: String
  }

parseSpecModule :: Path Rel File -> Maybe SpecModule
parseSpecModule rf = do
  let specModulePath = rf
  let specModuleModuleName = makeModuleName rf
  let withoutExtension = FP.dropExtension $ fromRelFile rf
  withoutSpecSuffix <- stripSuffix "Spec" withoutExtension
  withoutSpecSuffixPath <- parseRelFile withoutSpecSuffix
  let specModuleDescription = makeModuleName withoutSpecSuffixPath
  pure SpecModule {..}
  where
    stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
    stripSuffix suffix s = reverse <$> stripPrefix (reverse suffix) (reverse s)

makeModuleName :: Path Rel File -> String
makeModuleName fp =
  intercalate "." $ FP.splitDirectories $ FP.dropExtensions $ fromRelFile fp

makeSpecModule :: Settings -> Path Rel File -> [SpecModule] -> String
makeSpecModule Settings {..} destination sources =
  unlines
    [ if settingMain then "" else moduleDeclaration (makeModuleName destination),
      "",
      "import Test.Syd",
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

importDeclarations :: [SpecModule] -> String
importDeclarations = unlines . map (("import qualified " <>) . specModuleModuleName)

specDeclaration :: [SpecModule] -> String
specDeclaration fs =
  unlines $
    "spec :: Spec" :
    "spec = do" :
    map moduleSpecLine fs

moduleSpecLine :: SpecModule -> String
moduleSpecLine rf = unwords [" ", "describe", "\"" <> specModuleModuleName rf <> "\"", specFunctionName rf]

specFunctionName :: SpecModule -> String
specFunctionName rf = specModuleModuleName rf ++ ".spec"
