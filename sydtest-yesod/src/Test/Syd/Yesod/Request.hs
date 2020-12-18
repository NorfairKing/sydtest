{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Syd.Yesod.Request where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State (MonadState, StateT (..), execStateT)
import qualified Control.Monad.State as State
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.CaseInsensitive (CI)
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import GHC.Stack
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types as HTTP
import Test.Syd
import Test.Syd.Yesod.Client
import Text.Show.Pretty
import Web.Cookie as Cookie
import Yesod.Core as Yesod
import Yesod.Core.Unsafe

-- | Make a @GET@ request for the given route
get :: (Yesod site, RedirectUrl site url) => url -> YesodClientM site ()
get = performMethod methodGet

-- | Make a @POST@ request for the given route
post :: (Yesod site, RedirectUrl site url) => url -> YesodClientM site ()
post = performMethod methodPost

performMethod :: (Yesod site, RedirectUrl site url) => Method -> url -> YesodClientM site ()
performMethod method route = request $ do
  setUrl route
  setMethod method

statusIs :: HasCallStack => Int -> YesodClientM site ()
statusIs i = do
  mLastResp <- State.gets yesodClientStateLastResponse
  liftIO $ case mLastResp of
    Nothing -> expectationFailure "No request made yet."
    Just r ->
      let c = statusCode (responseStatus r)
       in shouldBeWith c i $
            unlines
              [ "full response:",
                ppShow r
              ]

newtype RequestBuilder site a = RequestBuilder
  { unRequestBuilder ::
      StateT
        (RequestBuilderData site)
        (ReaderT (YesodClient site) IO)
        a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (YesodClient site),
      MonadState (RequestBuilderData site),
      MonadFail,
      MonadThrow
    )

data RequestBuilderData site = RequestBuilderData
  { requestBuilderDataMethod :: !Method,
    requestBuilderDataUrl :: !Text,
    requestBuilderDataHeaders :: !HTTP.RequestHeaders,
    requestBuilderDataPostData :: !PostData,
    requestBuilderDataCookies :: !CookieJar
  }

data PostData
  = MultipleItemsPostData [RequestPart]
  | BinaryPostData ByteString

data RequestPart
  = ReqKvPart Text Text
  | ReqFilePart Text FilePath ByteString (Maybe Text)

initialRequestBuilderData :: CookieJar -> RequestBuilderData site
initialRequestBuilderData cj =
  RequestBuilderData
    { requestBuilderDataMethod = "GET",
      requestBuilderDataUrl = "",
      requestBuilderDataHeaders = [],
      requestBuilderDataPostData = MultipleItemsPostData [],
      requestBuilderDataCookies = cj
    }

isFile :: RequestPart -> Bool
isFile = \case
  ReqKvPart {} -> False
  ReqFilePart {} -> True

runRequestBuilder :: RequestBuilder site a -> YesodClientM site Request
runRequestBuilder (RequestBuilder func) = do
  client <- ask
  p <- asks yesodClientSitePort
  cj <- State.gets yesodClientStateCookies
  RequestBuilderData {..} <-
    liftIO $
      runReaderT
        ( execStateT
            func
            (initialRequestBuilderData cj)
        )
        client
  req <- liftIO $ parseRequest $ T.unpack requestBuilderDataUrl
  boundary <- liftIO webkitBoundary
  let (body, contentTypeHeader) = case requestBuilderDataPostData of
        MultipleItemsPostData dat ->
          if any isFile dat
            then
              ( runIdentity $
                  renderParts
                    boundary
                    ( flip map dat $ \case
                        ReqKvPart k v -> partBS k (TE.encodeUtf8 v)
                        ReqFilePart k path contents mime ->
                          (partFileRequestBody k path (RequestBodyBS contents))
                            { partContentType = TE.encodeUtf8 <$> mime
                            }
                    ),
                Just $ "multipart/form-data; boundary=" <> boundary
              )
            else
              ( RequestBodyBS $
                  renderSimpleQuery False $
                    flip mapMaybe dat $ \case
                      ReqKvPart k v -> Just (TE.encodeUtf8 k, TE.encodeUtf8 v)
                      ReqFilePart {} -> Nothing,
                Just "application/x-www-form-urlencoded"
              )
        BinaryPostData sb -> (RequestBodyBS sb, Nothing)
  now <- liftIO getCurrentTime
  let (req', cj') =
        insertCookiesIntoRequest
          ( req
              { port = p,
                method = requestBuilderDataMethod,
                requestHeaders =
                  concat
                    [ requestBuilderDataHeaders,
                      [("Content-Type", cth) | cth <- maybeToList contentTypeHeader]
                    ],
                requestBody = body
              }
          )
          cj
          now
  State.modify' (\s -> s {yesodClientStateCookies = cj'})
  pure req'

request :: RequestBuilder site a -> YesodClientM site ()
request rb = do
  req <- runRequestBuilder rb
  performRequest req

setUrl :: (Yesod site, RedirectUrl site url) => url -> RequestBuilder site ()
setUrl route = do
  site <- asks yesodClientSite
  Right url <-
    fmap ("http://localhost" <>)
      <$> Yesod.Core.Unsafe.runFakeHandler
        M.empty
        (const $ error "Test.Syd.Yesod: No logger available")
        site
        (toTextUrl route)
  State.modify'
    ( \oldReq ->
        oldReq
          { requestBuilderDataUrl = url
          }
    )

addRequestHeader :: HTTP.Header -> RequestBuilder site ()
addRequestHeader h = State.modify' (\r -> r {requestBuilderDataHeaders = h : requestBuilderDataHeaders r})

addGetParam :: Text -> Text -> RequestBuilder site ()
addGetParam = undefined

addPostParam :: Text -> Text -> RequestBuilder site ()
addPostParam name value =
  State.modify' $ \r -> r {requestBuilderDataPostData = addPostData (requestBuilderDataPostData r)}
  where
    addPostData (BinaryPostData _) = error "Trying to add post param to binary content."
    addPostData (MultipleItemsPostData posts) =
      MultipleItemsPostData $ ReqKvPart name value : posts

addFile ::
  -- | The parameter name for the file.
  Text ->
  -- | The path to the file.
  FilePath ->
  -- | The MIME type of the file, e.g. "image/png".
  Text ->
  RequestBuilder site ()
addFile name path mimetype = do
  contents <- liftIO $ SB.readFile path
  addFileWith name path contents (Just mimetype)

addFileWith ::
  -- | The parameter name for the file.
  Text ->
  -- | The path to the file.
  FilePath ->
  -- | The contents of the file.
  ByteString ->
  -- | The MIME type of the file, e.g. "image/png".
  Maybe Text ->
  RequestBuilder site ()
addFileWith name path contents mMimetype =
  State.modify' $ \r -> r {requestBuilderDataPostData = addPostData (requestBuilderDataPostData r)}
  where
    addPostData (BinaryPostData _) = error "Trying to add file after setting binary content."
    addPostData (MultipleItemsPostData posts) =
      MultipleItemsPostData $ ReqFilePart name path contents mMimetype : posts

setRequestBody :: ByteString -> RequestBuilder site ()
setRequestBody body = State.modify' $ \r -> r {requestBuilderDataPostData = BinaryPostData body}

-- | Look up the CSRF token from the given form data and add it to the request header
addToken_ :: HasCallStack => Query -> RequestBuilder site ()
addToken_ = undefined

-- | Look up the CSRF token from the only form data and add it to the request header
addToken :: HasCallStack => RequestBuilder site ()
addToken = undefined -- addToken_ ""

-- | Look up the CSRF token from the cookie with name 'defaultCsrfCookieName' and add it to the request header with name 'defaultCsrfHeaderName'.
addTokenFromCookie :: HasCallStack => RequestBuilder site ()
addTokenFromCookie = addTokenFromCookieNamedToHeaderNamed defaultCsrfCookieName defaultCsrfHeaderName

-- | Looks up the CSRF token stored in the cookie with the given name and adds it to the given request header.
addTokenFromCookieNamedToHeaderNamed ::
  HasCallStack =>
  -- | The name of the cookie
  ByteString ->
  -- | The name of the header
  CI ByteString ->
  RequestBuilder site ()
addTokenFromCookieNamedToHeaderNamed cookieName headerName = do
  cookies <- getRequestCookies
  case M.lookup cookieName cookies of
    Just csrfCookie -> addRequestHeader (headerName, setCookieValue csrfCookie)
    Nothing ->
      liftIO $
        expectationFailure $
          concat
            [ "addTokenFromCookieNamedToHeaderNamed failed to lookup CSRF cookie with name: ",
              show cookieName,
              ". Cookies were: ",
              show cookies
            ]

setMethod :: Method -> RequestBuilder site ()
setMethod m = State.modify' (\r -> r {requestBuilderDataMethod = m})

performRequest :: Request -> YesodClientM site ()
performRequest req = do
  man <- asks yesodClientManager
  resp <- liftIO $ httpLbs req man
  cj <- State.gets yesodClientStateCookies
  now <- liftIO getCurrentTime
  let (cj', _) = updateCookieJar resp req now cj
  State.modify'
    ( \s ->
        s
          { yesodClientStateLastResponse = Just resp,
            yesodClientStateCookies = cj'
          }
    )

-- | For backward compatibiilty, you can use the 'MonadState' constraint to get access to the 'CookieJar' directly.
getRequestCookies :: RequestBuilder site (Map ByteString Cookie.SetCookie)
getRequestCookies = undefined

-- | Query the last response using CSS selectors, returns a list of matched fragments
htmlQuery :: HasCallStack => Query -> YesodExample site [LB.ByteString]
htmlQuery = undefined
