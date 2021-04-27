{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- | Testing a yesod site.
--
-- For a fully-worked example, see sydtest-yesod/blog-example.
module Test.Syd.Yesod
  ( -- * Functions to run a test suite
    yesodSpec,
    yesodSpecWithSiteGenerator,
    yesodSpecWithSiteGeneratorAndArgument,
    yesodSpecWithSiteSupplier,
    yesodSpecWithSiteSupplierWith,
    yesodSpecWithSiteSetupFunc,
    yesodSpecWithSiteSetupFunc',

    -- *** Setup functions
    yesodClientSetupFunc,

    -- ** Core
    YesodSpec,
    YesodClient (..),
    YesodClientState (..),
    YesodClientM (..),
    runYesodClientM,
    YesodExample,

    -- * Helper functions to define tests
    yit,
    ydescribe,

    -- * Making requests
    get,
    post,
    followRedirect,

    -- ** Using the request builder
    request,
    setUrl,
    setMethod,
    addRequestHeader,
    addGetParam,
    addPostParam,
    addFile,
    addFileWith,
    setRequestBody,

    -- ** Helpers
    performMethod,
    performRequest,

    -- *** Types
    RequestBuilder (..),
    runRequestBuilder,

    -- *** Token
    addToken,
    addToken_,
    addTokenFromCookie,
    addTokenFromCookieNamedToHeaderNamed,

    -- *** Queries
    getRequest,
    getResponse,
    getLocation,
    getLast,

    -- * Declaring assertions
    statusIs,
    locationShouldBe,
    bodyContains,

    -- * Just to be sure we didn't forget any exports
    module Test.Syd.Yesod.Client,
    module Test.Syd.Yesod.Def,
    module Test.Syd.Yesod.Request,

    -- * Reexports
    module HTTP,
  )
where

import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Test.Syd.Yesod.Client
import Test.Syd.Yesod.Def
import Test.Syd.Yesod.Request
