{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Mutation.Plugin.OptParse
  ( MutationPluginConfig (..),
    defaultMutationPluginConfig,
    readMutationPluginConfigFile,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)

data MutationPluginConfig = MutationPluginConfig
  { mutationPluginConfigExceptions :: ![String],
    mutationPluginConfigDisabledMutations :: ![String],
    mutationPluginConfigSkipThSplices :: !Bool,
    mutationPluginConfigDebug :: !Bool
  }
  deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec MutationPluginConfig)

defaultMutationPluginConfig :: MutationPluginConfig
defaultMutationPluginConfig =
  MutationPluginConfig
    { mutationPluginConfigExceptions = [],
      mutationPluginConfigDisabledMutations = [],
      mutationPluginConfigSkipThSplices = False,
      mutationPluginConfigDebug = False
    }

instance HasCodec MutationPluginConfig where
  codec =
    object "MutationPluginConfig" $
      MutationPluginConfig
        <$> optionalFieldWithOmittedDefault
          "exceptions"
          (mutationPluginConfigExceptions defaultMutationPluginConfig)
          "Module names to skip during instrumentation"
          .= mutationPluginConfigExceptions
        <*> optionalFieldWithOmittedDefault
          "disabled-mutations"
          (mutationPluginConfigDisabledMutations defaultMutationPluginConfig)
          "Mutation operator names to disable globally"
          .= mutationPluginConfigDisabledMutations
        <*> optionalFieldWithOmittedDefault
          "skip-th-splices"
          (mutationPluginConfigSkipThSplices defaultMutationPluginConfig)
          "Skip mutations inside Template Haskell splices and quasi-quotes"
          .= mutationPluginConfigSkipThSplices
        <*> optionalFieldWithOmittedDefault
          "debug"
          (mutationPluginConfigDebug defaultMutationPluginConfig)
          "Print each mutation site as it is recorded"
          .= mutationPluginConfigDebug

readMutationPluginConfigFile :: FilePath -> IO MutationPluginConfig
readMutationPluginConfigFile = Yaml.decodeFileThrow
