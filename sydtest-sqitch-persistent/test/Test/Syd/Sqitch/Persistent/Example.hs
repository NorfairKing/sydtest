{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | A persistent model whose schema matches @test_resources/toy-sqitch-ok@.
module Test.Syd.Sqitch.Persistent.Example
  ( migrateWidget,
  )
where

import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateWidget"]
  [persistLowerCase|
Widget sql=widget
    name  Text       sqltype=text
    color Text Maybe sqltype=text
    deriving Show Eq
|]
