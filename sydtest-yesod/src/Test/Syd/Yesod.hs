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

    -- ** Tests against a local instance of a site
    yesodSpec,
    yesodSpecWithSiteGenerator,
    yesodSpecWithSiteGeneratorAndArgument,
    yesodSpecWithSiteSupplier,
    yesodSpecWithSiteSupplierWith,
    yesodSpecWithSiteSetupFunc,
    yesodSpecWithSiteSetupFuncWithMiddlewares,
    yesodSpecWithSiteSetupFunc',
    yesodSpecWithSiteSetupFuncWithMiddlewares',

    -- *** Setup functions
    yesodClientSetupFunc,
    yesodClientSetupFuncWithMiddlewares,

    -- ** Tests against a remote instance of a site
    yesodE2ESpec,
    yesodE2ESpec',
    E2E (..),
    localToE2ESpec,
    localToE2EClient,

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
    followRedirect_,

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
    getStatus,
    requireStatus,
    getRequest,
    requireRequest,
    getResponse,
    requireResponse,
    getLocation,
    requireLocation,
    getLast,
    requireLast,

    -- * Declaring assertions
    statusShouldBe,
    locationShouldBe,
    bodyContains,
    statusIs,

    -- * Just to be sure we didn't forget any exports
    module Test.Syd.Yesod.Client,
    module Test.Syd.Yesod.Def,
    module Test.Syd.Yesod.Request,
    module Test.Syd.Yesod.E2E,

    -- * Reexports
    module HTTP,
  )
where

import Network.HTTP.Client as HTTP
import Network.HTTP.Types as HTTP
import Test.Syd.Yesod.Client
import Test.Syd.Yesod.Def
import Test.Syd.Yesod.E2E
import Test.Syd.Yesod.Request
