{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Test a 'Wai.Application'
--
-- Example usage:
--
-- > exampleApplication :: Wai.Application
-- > exampleApplication req sendResp = do
-- >   lb <- strictRequestBody req
-- >   sendResp $ responseLBS HTTP.ok200 (requestHeaders req) lb
-- >
-- > spec :: Spec
-- > spec =
-- >   waiClientSpec exampleApplication $
-- >     describe "get" $
-- >       wit "can GET the root and get a 200" $ do
-- >         resp <- get "/"
-- >         liftIO $ responseStatus resp `shouldBe` ok200
module Test.Syd.Wai
  ( -- * Functions to run a test suite

    -- ** A test suite that uses a running wai applications
    waiSpec,
    waiSpecWith,
    waiSpecWith',
    waiSpecWithSetupFunc,

    -- ** A test suite that uses a running wai application and calls it using the functions provided in this package
    waiClientSpec,
    waiClientSpecWith,
    waiClientSpecWithSetupFunc,
    waiClientSpecWithSetupFunc',
    wit,

    -- ** A test suite that uses a single HTTP manager accross tests
    managerSpec,

    -- *** Setup functions
    waiClientSetupFunc,
    applicationSetupFunc,

    -- ** Core
    WaiClient (..),
    WaiClientState (..),
    WaiClientM (..),
    runWaiClientM,

    -- * Making requests
    get,
    post,
    put,
    patch,
    options,
    delete,
    request,
    performRequest,

    -- * Assertions
    ResponseMatcher (..),
    MatchHeader (..),
    MatchBody (..),
    shouldRespondWith,
    Body,
    (<:>),

    -- * Just to make sure we didn't forget any exports
    module Test.Syd.Wai.Client,
    module Test.Syd.Wai.Def,
    module Test.Syd.Wai.Request,

    -- * Reexports
    module HTTP,
  )
where

import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Test.Syd.Wai.Client
import Test.Syd.Wai.Def
import Test.Syd.Wai.Matcher
import Test.Syd.Wai.Request
