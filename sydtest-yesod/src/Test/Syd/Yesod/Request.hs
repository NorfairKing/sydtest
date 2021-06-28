{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints -fno-warn-unused-imports #-}

module Test.Syd.Yesod.Request where

import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Fail
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
import Network.HTTP.Client.Internal (httpRaw)
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Types as HTTP
import Test.Syd
import Test.Syd.Yesod.Client
import qualified Text.XML.Cursor as C
import Web.Cookie as Cookie
import Yesod.Core as Yesod
import Yesod.Core.Unsafe
import qualified Yesod.Test as YesodTest
import Yesod.Test.TransversingCSS as CSS

-- | Make a @GET@ request for the given route
--
-- > it "returns 200 on the home route" $ do
-- >   get HomeR
-- >   statusIs 200
get :: (Yesod site, RedirectUrl site url) => url -> YesodClientM site ()
get = performMethod methodGet

-- | Make a @POST@ request for the given route
--
-- > it "returns 200 on the start processing route" $ do
-- >   post StartProcessingR
-- >   statusIs 200
post :: (Yesod site, RedirectUrl site url) => url -> YesodClientM site ()
post = performMethod methodPost

-- | Perform a request using an arbitrary method for the given route.
performMethod :: (Yesod site, RedirectUrl site url) => Method -> url -> YesodClientM site ()
performMethod method route = request $ do
  setUrl route
  setMethod method

-- | Assert the status of the most recently received response.
--
-- > it "returns 200 on the home route" $ do
-- >   get HomeR
-- >   statusIs 200
statusIs :: HasCallStack => Int -> YesodClientM site ()
statusIs i = do
  mLast <- getLast
  case mLast of
    Nothing -> liftIO $ expectationFailure "statusIs: No request made yet."
    Just (_, resp) ->
      let c = statusCode (responseStatus resp)
       in withLastRequestContext $ liftIO $ c `shouldBe` i

-- | Assert the redirect location of the most recently received response.
--
-- > it "redirects to the overview on the home route" $ do
-- >   get HomeR
-- >   statusIs 301
-- >   locationShouldBe OverviewR
locationShouldBe :: (ParseRoute site, Show (Route site)) => Route site -> YesodClientM site2 ()
locationShouldBe expected =
  withLastRequestContext $ do
    errOrLoc <- getLocation
    liftIO $ case errOrLoc of
      Left err -> expectationFailure (T.unpack err)
      Right actual -> actual `shouldBe` expected

-- | Assert the last response has the given text.
--
-- The check is performed using the response body in full text form without any html parsing.
bodyContains :: HasCallStack => String -> YesodExample site ()
bodyContains text = do
  mResp <- getLast
  case mResp of
    Nothing -> liftIO $ expectationFailure "bodyContains: No request made yet."
    Just (_, resp) ->
      withLastRequestContext $
        liftIO $
          shouldSatisfyNamed (responseBody resp) (unwords ["bodyContains", show text]) (\body -> TE.encodeUtf8 (T.pack text) `SB.isInfixOf` LB.toStrict body)

-- | A request builder monad that allows you to monadically build a request using `runRequestBuilder`.
--
-- This request builder has access to the entire `YesodClientM` underneath.
-- This includes the `Site` under test, as well as cookies etc.
--
-- See 'YesodClientM' for more details.
newtype RequestBuilder site a = RequestBuilder
  { unRequestBuilder ::
      StateT
        (RequestBuilderData site)
        (YesodClientM site)
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

-- | Run a 'YesodClientM' function as part of a 'RequestBuilder'.
liftClient :: YesodClientM site a -> RequestBuilder site a
liftClient = RequestBuilder . lift

data RequestBuilderData site = RequestBuilderData
  { requestBuilderDataMethod :: !Method,
    requestBuilderDataUrl :: !Text,
    requestBuilderDataHeaders :: !HTTP.RequestHeaders,
    requestBuilderDataGetParams :: !HTTP.Query,
    requestBuilderDataPostData :: !PostData
  }

data PostData
  = MultipleItemsPostData [RequestPart]
  | BinaryPostData ByteString

data RequestPart
  = ReqKvPart Text Text
  | ReqFilePart Text FilePath ByteString (Maybe Text)

initialRequestBuilderData :: RequestBuilderData site
initialRequestBuilderData =
  RequestBuilderData
    { requestBuilderDataMethod = "GET",
      requestBuilderDataUrl = "",
      requestBuilderDataHeaders = [],
      requestBuilderDataGetParams = [],
      requestBuilderDataPostData = MultipleItemsPostData []
    }

isFile :: RequestPart -> Bool
isFile = \case
  ReqKvPart {} -> False
  ReqFilePart {} -> True

-- | Run a 'RequestBuilder' to make the 'Request' that it defines.
runRequestBuilder :: RequestBuilder site a -> YesodClientM site Request
runRequestBuilder (RequestBuilder func) = do
  p <- asks yesodClientSitePort
  cj <- State.gets yesodClientStateCookies
  RequestBuilderData {..} <- execStateT func initialRequestBuilderData
  let requestStr = T.unpack requestBuilderDataUrl

  req <- case parseRequest requestStr <|> parseRequest ("http://localhost" <> requestStr) of
    Nothing -> liftIO $ expectationFailure $ "Failed to parse url: " <> requestStr
    Just req -> pure req
  boundary <- liftIO webkitBoundary
  (body, contentTypeHeader) <- liftIO $ case requestBuilderDataPostData of
    MultipleItemsPostData [] -> pure (RequestBodyBS SB.empty, Nothing)
    MultipleItemsPostData dat ->
      if any isFile dat
        then do
          ps <-
            renderParts
              boundary
              ( flip map dat $ \case
                  ReqKvPart k v -> partBS k (TE.encodeUtf8 v)
                  ReqFilePart k path contents mime ->
                    (partFileRequestBody k path (RequestBodyBS contents))
                      { partContentType = TE.encodeUtf8 <$> mime
                      }
              )
          pure
            ( ps,
              Just $ "multipart/form-data; boundary=" <> boundary
            )
        else
          pure
            ( RequestBodyBS $
                renderSimpleQuery False $
                  flip mapMaybe dat $ \case
                    ReqKvPart k v -> Just (TE.encodeUtf8 k, TE.encodeUtf8 v)
                    ReqFilePart {} -> Nothing,
              Just "application/x-www-form-urlencoded"
            )
    BinaryPostData sb -> pure (RequestBodyBS sb, Nothing)
  now <- liftIO getCurrentTime
  let (req', cj') =
        insertCookiesIntoRequest
          ( req
              { port = fromIntegral p, -- Safe because it is PortNumber -> Int
                method = requestBuilderDataMethod,
                requestHeaders =
                  concat
                    [ requestBuilderDataHeaders,
                      [("Content-Type", cth) | cth <- maybeToList contentTypeHeader]
                    ],
                requestBody = body,
                queryString = HTTP.renderQuery False requestBuilderDataGetParams
              }
          )
          cj
          now
  State.modify' (\s -> s {yesodClientStateCookies = cj'})
  pure req'

-- | Perform the request that is built by the given 'RequestBuilder'.
--
-- > it "returns 200 on this post request" $ do
-- >   request $ do
-- >     setUrl StartProcessingR
-- >     setMethod "POST"
-- >     addPostParam "key" "value"
-- >   statusIs 200
request :: RequestBuilder site a -> YesodClientM site ()
request rb = do
  req <- runRequestBuilder rb
  performRequest req

-- | Set the url of the 'RequestBuilder' to the given route.
setUrl :: (Yesod site, RedirectUrl site url) => url -> RequestBuilder site ()
setUrl route = do
  site <- asks yesodClientSite
  Right url <-
    Yesod.Core.Unsafe.runFakeHandler
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

-- | Set the method of the 'RequestBuilder'.
setMethod :: Method -> RequestBuilder site ()
setMethod m = State.modify' (\r -> r {requestBuilderDataMethod = m})

-- | Add the given request header to the 'RequestBuilder'.
addRequestHeader :: HTTP.Header -> RequestBuilder site ()
addRequestHeader h = State.modify' (\r -> r {requestBuilderDataHeaders = h : requestBuilderDataHeaders r})

-- | Add the given GET parameter to the 'RequestBuilder'.
addGetParam :: Text -> Text -> RequestBuilder site ()
addGetParam k v = State.modify' (\r -> r {requestBuilderDataGetParams = (TE.encodeUtf8 k, Just $ TE.encodeUtf8 v) : requestBuilderDataGetParams r})

-- | Add the given POST parameter to the 'RequestBuilder'.
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

-- | Set the request body of the 'RequestBuilder'.
--
-- Note that this invalidates any of the other post parameters that may have been set.
setRequestBody :: ByteString -> RequestBuilder site ()
setRequestBody body = State.modify' $ \r -> r {requestBuilderDataPostData = BinaryPostData body}

-- | Look up the CSRF token from the given form data and add it to the request header
addToken_ :: HasCallStack => Text -> RequestBuilder site ()
addToken_ scope = do
  matches <- liftClient $ htmlQuery $ scope <> " input[name=_token][type=hidden][value]"
  case matches of
    [] -> liftIO $ expectationFailure "No CSRF token found in the current page"
    [element] -> addPostParam "_token" $ head $ C.attribute "value" $ YesodTest.parseHTML element
    _ -> liftIO $ expectationFailure "More than one CSRF token found in the page"

-- | Look up the CSRF token from the only form data and add it to the request header
addToken :: HasCallStack => RequestBuilder site ()
addToken = addToken_ ""

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

-- | Perform the given request as-is.
--
-- Note that this function does not check whether you are making a request to the site under test.
-- You could make a request to https://google.com if you wanted.
performRequest :: Request -> YesodClientM site ()
performRequest req = do
  man <- asks yesodClientManager
  resp <- liftIO $ httpRaw req man >>= traverse (fmap LB.fromChunks . brConsume)
  cj <- State.gets yesodClientStateCookies
  now <- liftIO getCurrentTime
  let (cj', _) = updateCookieJar resp req now cj
  State.modify'
    ( \s ->
        s
          { yesodClientStateLast = Just (req, resp),
            yesodClientStateCookies = cj'
          }
    )

-- | For backward compatibiilty, you can use the 'MonadState' constraint to get access to the 'CookieJar' directly.
getRequestCookies :: RequestBuilder site (Map ByteString SetCookie)
getRequestCookies = do
  cj <- liftClient $ State.gets yesodClientStateCookies
  pure $
    M.fromList $
      flip map (destroyCookieJar cj) $ \Cookie {..} ->
        ( cookie_name,
          defaultSetCookie
            { setCookieName = cookie_name,
              setCookieValue = cookie_value
            }
        )

-- | Query the last response using CSS selectors, returns a list of matched fragments
htmlQuery :: HasCallStack => CSS.Query -> YesodExample site [CSS.HtmlLBS]
htmlQuery query = do
  mResp <- getResponse
  case mResp of
    Nothing -> liftIO $ expectationFailure "No request made yet."
    Just resp -> case CSS.findBySelector (responseBody resp) query of
      Left err -> liftIO $ expectationFailure $ show query <> " did not parse: " <> show err
      Right matches -> pure $ map (LB.fromStrict . TE.encodeUtf8 . T.pack) matches

-- | Follow a redirect, if the last response was a redirect.
--
-- (We consider a request a redirect if the status is
-- 301, 302, 303, 307 or 308, and the Location header is set.)
followRedirect ::
  Yesod site =>
  -- | 'Left' with an error message if not a redirect, 'Right' with the redirected URL if it was
  YesodExample site (Either Text Text)
followRedirect = do
  mr <- getResponse
  case mr of
    Nothing -> return $ Left "followRedirect called, but there was no previous response, so no redirect to follow"
    Just r -> do
      if HTTP.statusCode (responseStatus r) `notElem` [301, 302, 303, 307, 308]
        then return $ Left "followRedirect called, but previous request was not a redirect"
        else do
          case lookup "Location" (responseHeaders r) of
            Nothing -> return $ Left "followRedirect called, but no location header set"
            Just h ->
              let url = TE.decodeUtf8 h
               in get url >> return (Right url)
