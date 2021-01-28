{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Example.Blog where

import Control.Monad.Logger
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Yesod

share
  [mkPersist sqlSettings, mkMigrate "migrateThoughts"]
  [persistLowerCase|
Thought
    title Text
    contents Text
    deriving Show Eq
|]

data App = App
  { appConnectionPool :: ConnectionPool
  }

mkYesod
  "App"
  [parseRoutes|
    / HomeR GET
    /new-thought NewThoughtR GET POST
    /thought/#ThoughtId ThoughtR GET
|]

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

instance Yesod App

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

runDB :: SqlPersistT IO a -> Handler a
runDB func = do
  pool <- getsYesod appConnectionPool
  liftIO $ runSqlPool func pool

getHomeR :: Handler Html
getHomeR = defaultLayout "Welcome!, feel free to post some thoughts at /thought."

data NewThought = NewThought
  { newThoughtTitle :: Text,
    newThoughtContents :: Text
  }

newThoughtForm :: Form NewThought
newThoughtForm =
  renderDivs $
    NewThought
      <$> areq textField ("Title" {fsName = Just "title"}) Nothing
      <*> (unTextarea <$> areq textareaField ("Contents" {fsName = Just "contents"}) Nothing)

getNewThoughtR :: Handler Html
getNewThoughtR = do
  ((res, form), enctype) <- runFormPost newThoughtForm
  case res of
    FormSuccess f ->
      defaultLayout
        [whamlet|
          <p>You've posted a thought!
          <p>the title was #{newThoughtTitle f}
          <p>the contents were #{Textarea $ newThoughtContents f}
        |]
    FormMissing ->
      defaultLayout
        [whamlet|
          <p>Hello world!
          <form enctype="#{enctype}" method="post">
            ^{form}
            <input type="submit">
              Submit thought
        |]
    FormFailure errs -> invalidArgs errs

postNewThoughtR :: Handler Html
postNewThoughtR = getNewThoughtR

getThoughtR :: ThoughtId -> Handler Html
getThoughtR = undefined

main :: IO ()
main = runStderrLoggingT $
  withSqlitePool "example.sqlite3" 1 $ \pool -> do
    runSqlPool (runMigration migrateThoughts) pool
    liftIO $ Yesod.warp 3000 $ App {appConnectionPool = pool}
