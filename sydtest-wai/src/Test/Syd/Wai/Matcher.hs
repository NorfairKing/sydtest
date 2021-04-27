{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | This entire module only serves to be backwards compatible with Test.Hspec.Wai.Matcher
--
-- This approach of asserting what the response looks like is obsolete because of the way sydtest does things.
-- You should use `shouldBe` instead.
module Test.Syd.Wai.Matcher where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.CaseInsensitive as CI
import Data.Char as Char (isPrint, isSpace)
import Data.Maybe
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types as HTTP

type Body = LB.ByteString

data ResponseMatcher = ResponseMatcher
  { matchStatus :: Int,
    matchHeaders :: [MatchHeader],
    matchBody :: MatchBody
  }

data MatchHeader = MatchHeader ([Header] -> Body -> Maybe String)

data MatchBody = MatchBody ([Header] -> Body -> Maybe String)

bodyEquals :: Body -> MatchBody
bodyEquals body = MatchBody (\_ actual -> bodyMatcher actual body)
  where
    bodyMatcher :: Body -> Body -> Maybe String
    bodyMatcher (LB.toStrict -> actual) (LB.toStrict -> expected) = actualExpected "body mismatch:" actual_ expected_ <$ guard (actual /= expected)
      where
        (actual_, expected_) = case (safeToString actual, safeToString expected) of
          (Just x, Just y) -> (x, y)
          _ -> (show actual, show expected)

matchAny :: MatchBody
matchAny = MatchBody (\_ _ -> Nothing)

instance IsString MatchBody where
  fromString = bodyEquals . LB.fromStrict . TE.encodeUtf8 . T.pack

instance IsString ResponseMatcher where
  fromString = ResponseMatcher 200 [] . fromString

instance Num ResponseMatcher where
  fromInteger n = ResponseMatcher (fromInteger n) [] matchAny
  (+) = error "ResponseMatcher does not support (+)"
  (-) = error "ResponseMatcher does not support (-)"
  (*) = error "ResponseMatcher does not support (*)"
  abs = error "ResponseMatcher does not support `abs`"
  signum = error "ResponseMatcher does not support `signum`"

(<:>) :: HeaderName -> ByteString -> MatchHeader
name <:> value = MatchHeader $ \headers _body ->
  guard (header `notElem` headers)
    >> (Just . unlines)
      [ "missing header:",
        formatHeader header
      ]
  where
    header = (name, value)

actualExpected :: String -> String -> String -> String
actualExpected message actual expected =
  unlines
    [ message,
      "  expected: " ++ expected,
      "  but got:  " ++ actual
    ]

formatHeader :: Header -> String
formatHeader header@(name, value) = "  " ++ fromMaybe (show header) (safeToString $ B8.concat [CI.original name, ": ", value])

safeToString :: ByteString -> Maybe String
safeToString bs = do
  str <- either (const Nothing) (Just . T.unpack) (TE.decodeUtf8' bs)
  let isSafe = not $ case str of
        [] -> True
        _ -> Char.isSpace (last str) || not (all Char.isPrint str)
  guard isSafe >> return str
