{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Test.Syd.Yesod
  ( -- * Functions to run a test suite
    yesodSpec,
    yesodSpecWithSiteGenerator,
    yesodSpecWithSiteGeneratorAndArgument,
    yesodSpecWithSiteSupplier,
    yesodSpecWithSiteSupplierWith,

    -- ** Core
    YesodSpec,
    YesodClient (..),
    YesodClientM (..),
    runYesodClientM,
    YesodExample,

    -- ** Helper functions in case you want to do something fancy
    yesodSpecWithFunc,

    -- * Helper functions to define tests
    yit,
    ydescribe,

    -- * Making requests
    get,
    post,
    performMethod,
    performRequest,

    -- ** Using the request builder
    request,
    setUrl,
    setMethod,
    addRequestHeader,
    addPostParam,
    RequestBuilder (..),
    runRequestBuilder,
    getLocation,

    -- *** Token
    addToken,
    addToken_,
    addTokenFromCookie,
    addTokenFromCookieNamedToHeaderNamed,

    -- * Declaring assertions
    statusIs,

    -- ** Reexports
    module HTTP,
  )
where

import qualified Blaze.ByteString.Builder as Builder
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State (MonadState, StateT (..), evalStateT, execStateT)
import qualified Control.Monad.State as State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Stack
import Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Network.Wai.Handler.Warp as Warp
import Test.Syd
import Web.Cookie as Cookie
import Yesod.Core as Yesod
import Yesod.Core.Unsafe

-- | Run a test suite using the given 'site'.
--
-- If your 'site' contains any resources that need to be set up, you probably want to be using one of the following functions instead.
--
-- This function exists for backward compatibility with yesod-test.
yesodSpec :: YesodDispatch site => site -> YesodSpec site -> Spec
yesodSpec site = yesodSpecWithSiteGenerator $ pure site

-- | Run a test suite using the given 'site' generator.
--
-- If your 'site' contains any resources that you will want to have set up beforhand, you will probably want to use 'yesodSpecWithSiteGeneratorAndArgument' or 'yesodSpecWithSiteSupplierWith' instead.
--
-- This function exists for backward compatibility with yesod-test.
yesodSpecWithSiteGenerator :: YesodDispatch site => IO site -> YesodSpec site -> Spec
yesodSpecWithSiteGenerator siteGen = yesodSpecWithSiteGeneratorAndArgument $ \() -> siteGen

-- | Run a test suite using the given 'site' generator which uses an inner resource.
--
-- If your 'site' contains any resources that you need to set up using a 'withX' function, you will want to use `yesodSpecWithSiteSupplier` instead.
--
-- This function exists for backward compatibility with yesod-test.
yesodSpecWithSiteGeneratorAndArgument :: YesodDispatch site => (a -> IO site) -> YesodSpec site -> SpecWith a
yesodSpecWithSiteGeneratorAndArgument func = yesodSpecWithSiteSupplierWith $ \f a -> func a >>= f

-- | Using a function that supplies a 'site', run a test suite.
yesodSpecWithSiteSupplier :: YesodDispatch site => ((site -> IO ()) -> IO ()) -> YesodSpec site -> Spec
yesodSpecWithSiteSupplier func = yesodSpecWithSiteSupplierWith (\f () -> func f)

-- | Using a function that supplies a 'site', based on an inner resource, run a test suite.
yesodSpecWithSiteSupplierWith :: YesodDispatch site => ((site -> IO ()) -> (a -> IO ())) -> YesodSpec site -> SpecWith a
yesodSpecWithSiteSupplierWith func = aroundWith func . beforeAll (newManager defaultManagerSettings) . aroundWith' yesodSpecWithFunc

-- | Turn a function that takes a 'YesodClient site' into a function that only takes a 'site'.
yesodSpecWithFunc :: YesodDispatch site => (HTTP.Manager -> YesodClient site -> IO ()) -> (HTTP.Manager -> site -> IO ())
yesodSpecWithFunc func man site =
  Warp.testWithApplication (Yesod.toWaiAppPlain site) $ \port ->
    let client =
          YesodClient
            { yesodClientManager = man,
              yesodClientSite = site,
              yesodClientSitePort = port
            }
     in func man client

-- | For backward compatibility with yesod-test
type YesodSpec site = TestDefM '[HTTP.Manager] (YesodClient site) ()

-- | A client environment to call a Yesod app.
data YesodClient site = YesodClient
  { -- | The site itself
    yesodClientSite :: !site,
    -- | The 'HTTP.Manager' to make the requests
    yesodClientManager :: !HTTP.Manager,
    -- | The port that the site is running on, using @warp@
    yesodClientSitePort :: !Int
  }

data YesodClientState site = YesodClientState
  { -- | The last response received
    yesodClientStateLastResponse :: !(Maybe (Response LB.ByteString)),
    -- | The cookies to pass along
    yesodClientStateCookies :: !Cookies
  }

initYesodClientState :: YesodClientState site
initYesodClientState =
  YesodClientState
    { yesodClientStateLastResponse = Nothing,
      yesodClientStateCookies = []
    }

-- | A monad to call a Yesod app.
--
-- This has access to a 'YesodClient site'.
newtype YesodClientM site a = YesodClientM
  { unYesodClientM :: StateT (YesodClientState site) (ReaderT (YesodClient site) IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (YesodClient site),
      MonadState (YesodClientState site),
      MonadFail,
      MonadThrow
    )

-- | For backward compatibility
type YesodExample site a = YesodClientM site a

-- | Run a YesodClientM site using a YesodClient site
runYesodClientM :: YesodClient site -> YesodClientM site a -> IO a
runYesodClientM cenv (YesodClientM func) = runReaderT (evalStateT func initYesodClientState) cenv

-- | Define a test in the 'YesodClientM site' monad instead of 'IO'.
yit :: forall site. HasCallStack => String -> YesodClientM site () -> YesodSpec site
yit s f = it s ((\cenv -> runYesodClientM cenv f) :: YesodClient site -> IO ())

-- | For backward compatibility
ydescribe :: String -> YesodSpec site -> YesodSpec site
ydescribe = describe

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
    Just r -> statusCode (responseStatus r) `shouldBe` i

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
    requestBuilderDataPostData :: !PostData
  }

data PostData
  = MultipleItemsPostData [RequestPart]
  | BinaryPostData ByteString

data RequestPart
  = ReqKvPart Text Text
  | ReqFilePart Text FilePath ByteString Text

initialRequestBuilderData :: RequestBuilderData site
initialRequestBuilderData =
  RequestBuilderData
    { requestBuilderDataMethod = "GET",
      requestBuilderDataUrl = "",
      requestBuilderDataHeaders = [],
      requestBuilderDataPostData = MultipleItemsPostData []
    }

runRequestBuilder :: RequestBuilder site a -> YesodClientM site Request
runRequestBuilder (RequestBuilder func) = do
  client <- ask
  p <- asks yesodClientSitePort
  RequestBuilderData {..} <-
    liftIO $
      runReaderT
        ( execStateT
            func
            initialRequestBuilderData
        )
        client
  req <- liftIO $ parseRequest $ T.unpack requestBuilderDataUrl
  pure $
    req
      { port = p,
        method = requestBuilderDataMethod,
        requestHeaders = requestBuilderDataHeaders
      }

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

addPostParam :: Text -> Text -> RequestBuilder site ()
addPostParam name value =
  State.modify' $ \r -> r {requestBuilderDataPostData = addPostData (requestBuilderDataPostData r)}
  where
    addPostData (BinaryPostData _) = error "Trying to add post param to binary content."
    addPostData (MultipleItemsPostData posts) =
      MultipleItemsPostData $ ReqKvPart name value : posts

addGetParam :: Text -> Text -> RequestBuilder site ()
addGetParam = undefined

-- | Look up the CSRF token from the given form data and add it to the request header
addToken_ :: HasCallStack => Query -> RequestBuilder site ()
addToken_ scope = undefined

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
  case lookup cookieName cookies of
    Just csrfCookie -> addRequestHeader (headerName, csrfCookie)
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
  State.modify' (\s -> s {yesodClientStateLastResponse = Just resp})

getRequestCookies :: RequestBuilder site Cookies
getRequestCookies = undefined

getResponse :: YesodClientM site (Maybe (Response LB.ByteString))
getResponse = State.gets yesodClientStateLastResponse

-- | Query the last response using CSS selectors, returns a list of matched fragments
htmlQuery :: HasCallStack => Query -> YesodExample site [LB.ByteString]
htmlQuery = undefined

-- -- | Query the last response using CSS selectors, returns a list of matched fragments
-- htmlQuery' ::
--   HasCallStack =>
--   (state -> Maybe (Response LB.ByteString)) ->
--   [Text] ->
--   Query ->
--   SIO state [HtmlLBS]
-- htmlQuery' getter errTrace query = withResponse' getter ("Tried to invoke htmlQuery' in order to read HTML of a previous response." : errTrace) $ \res ->
--   case findBySelector (simpleBody res) query of
--     Left err -> failure $ query <> " did not parse: " <> T.pack (show err)
--     Right matches -> return $ map (encodeUtf8 . TL.pack) matches

-- | Use HXT to parse a value from an HTML tag.
-- Check for usage examples in this module's source.
-- parseHTML :: HtmlLBS -> Cursor
-- parseHTML html = fromDocument $ HD.parseLBS html
getLocation :: ParseRoute site => YesodExample site (Either Text (Route site))
getLocation = do
  mr <- getResponse
  case mr of
    Nothing -> return $ Left "getLocation called, but there was no previous response, so no Location header"
    Just r -> case lookup "Location" (responseHeaders r) of
      Nothing -> return $ Left "getLocation called, but the previous response has no Location header"
      Just h -> case parseRoute $ decodePath h of
        Nothing -> return $ Left "getLocation called, but couldn’t parse it into a route"
        Just l -> return $ Right l
  where
    decodePath b =
      let (x, y) = SB8.break (== '?') b
       in (HTTP.decodePathSegments x, unJust <$> HTTP.parseQueryText y)
    unJust (a, Just b) = (a, b)
    unJust (a, Nothing) = (a, mempty)
