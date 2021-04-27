{-# LANGUAGE OverloadedStrings #-}

module Test.Syd.Wai.Request where

import Control.Monad.Reader
import Control.Monad.State as State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Time
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal (httpRaw)
import Network.HTTP.Types as HTTP
import Test.Syd.Wai.Client

-- | Perform a @GET@ request to the application under test.
get :: ByteString -> WaiSession st (HTTP.Response LB.ByteString)
get path = request methodGet path [] ""

-- | Perform a @POST@ request to the application under test.
post :: ByteString -> LB.ByteString -> WaiSession st (HTTP.Response LB.ByteString)
post path = request methodPost path []

-- | Perform a @PUT@ request to the application under test.
put :: ByteString -> LB.ByteString -> WaiSession st (HTTP.Response LB.ByteString)
put path = request methodPut path []

-- | Perform a @PATCH@ request to the application under test.
patch :: ByteString -> LB.ByteString -> WaiSession st (HTTP.Response LB.ByteString)
patch path = request methodPatch path []

-- | Perform an @OPTIONS@ request to the application under test.
options :: ByteString -> WaiSession st (HTTP.Response LB.ByteString)
options path = request methodOptions path [] ""

-- | Perform a @DELETE@ request to the application under test.
delete :: ByteString -> WaiSession st (HTTP.Response LB.ByteString)
delete path = request methodDelete path [] ""

-- | Perform a request to the application under test, with specified HTTP
-- method, request path, headers and body.
request :: Method -> ByteString -> [Header] -> LB.ByteString -> WaiSession st (HTTP.Response LB.ByteString)
request method path headers body = do
  let req =
        defaultRequest
          { method = method,
            path = path,
            requestHeaders = headers,
            requestBody = RequestBodyLBS body
          }
  performRequest req

performRequest :: HTTP.Request -> WaiSession st (HTTP.Response LB.ByteString)
performRequest req = do
  man <- asks waiClientManager
  resp <- liftIO $ httpRaw req man >>= traverse (fmap LB.fromChunks . brConsume)
  cj <- State.gets waiClientStateCookies
  now <- liftIO getCurrentTime
  let (cj', _) = updateCookieJar resp req now cj
  State.modify'
    ( \s ->
        s
          { waiClientStateLast = Just (req, resp),
            waiClientStateCookies = cj'
          }
    )
  pure resp
