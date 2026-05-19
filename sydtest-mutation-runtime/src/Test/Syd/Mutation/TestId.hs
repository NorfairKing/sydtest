{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Test.Syd.Mutation.TestId
  ( TestId (..),
    renderTestId,
    parseTestIdFilterArg,
  )
where

import Autodocodec
import Data.GenValidity
import Data.GenValidity.Text ()
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | An opaque identifier for a single test in a 'Spec'.
--
-- A 'TestId' is stable across runs as long as the spec structure does not
-- change.  When two tests share the same description at the same level, a
-- zero-based per-description sibling index is used to distinguish them; the
-- index is omitted when it is zero.
newtype TestId = TestId (NonEmpty (Text, Word))
  deriving (Eq, Ord, Show, Generic)

-- | Render a 'TestId' as a human-readable string that is also parseable by
-- 'parseTestIdFilterArg' and usable as the argument to @--filter-id@.
--
-- Steps are separated by @.@.  The index suffix @:n@ (for @n > 0@) is
-- appended after the description.  Literal @\\@, @.@, and @:@ in
-- descriptions are escaped as @\\\\@, @\\.@, and @\\:@.
renderTestId :: TestId -> Text
renderTestId (TestId steps) = T.intercalate "." (map renderStep (NE.toList steps))
  where
    renderStep (t, 0) = escapeDesc t
    renderStep (t, n) = escapeDesc t <> ":" <> T.pack (show (n :: Word))

escapeDesc :: Text -> Text
escapeDesc = T.concatMap $ \case
  '\\' -> "\\\\"
  '.' -> "\\."
  ':' -> "\\:"
  c -> T.singleton c

-- | Reverse the escape table used by 'escapeDesc'.  Only @\\\\@, @\\.@, and
-- @\\:@ are valid escape sequences: 'escapeDesc' never produces anything
-- else, so accepting other @\\<c>@ sequences would make the parser accept
-- inputs that no 'renderTestId' call can produce — a forward
-- round-trip violation.  Returns 'Nothing' on an unknown escape or a
-- trailing lone backslash.
unescapeDesc :: Text -> Maybe Text
unescapeDesc t = T.pack <$> go (T.unpack t)
  where
    go [] = Just []
    go ('\\' : c : rest)
      | c == '\\' || c == '.' || c == ':' = (c :) <$> go rest
      | otherwise = Nothing
    go ['\\'] = Nothing
    go (c : rest) = (c :) <$> go rest

-- | Parse the output of 'renderTestId' back into a 'TestId'.
-- Returns 'Nothing' if the input is malformed or empty.
parseTestIdFilterArg :: Text -> Maybe TestId
parseTestIdFilterArg t
  | T.null t = Nothing
  | otherwise = do
      steps <- parseSteps (T.unpack t)
      TestId <$> NE.nonEmpty steps

-- | Split on unescaped dots and parse each raw (still-escaped) step.
parseSteps :: String -> Maybe [(Text, Word)]
parseSteps = fmap reverse . go [] []
  where
    go steps acc [] =
      (: steps) <$> finishStep (reverse acc)
    go steps acc ('\\' : c : rest) =
      go steps (c : '\\' : acc) rest
    go _ _ ['\\'] = Nothing
    go steps acc ('.' : rest) = do
      step <- finishStep (reverse acc)
      go (step : steps) [] rest
    go steps acc (c : rest) =
      go steps (c : acc) rest

    finishStep :: String -> Maybe (Text, Word)
    finishStep [] = Nothing
    finishStep s =
      case splitAtLastUnescapedColon s of
        Just (rawName, idxStr)
          | not (null idxStr) ->
              case reads idxStr of
                [(n, "")] -> (,n) <$> unescapeDesc (T.pack rawName)
                _ -> (,0) <$> unescapeDesc (T.pack s)
        _ -> (,0) <$> unescapeDesc (T.pack s)

    splitAtLastUnescapedColon :: String -> Maybe (String, String)
    splitAtLastUnescapedColon s = case lastUnescapedColon s of
      Nothing -> Nothing
      Just i -> Just (take i s, drop (i + 1) s)

    -- \| Index of the last unescaped @:@ in @s@, or 'Nothing' if there is none.
    lastUnescapedColon :: String -> Maybe Int
    lastUnescapedColon = scan 0 Nothing False
      where
        scan _ acc _ [] = acc
        scan i acc True (_ : rest) = scan (i + 1) acc False rest
        scan i acc False ('\\' : rest) = scan (i + 1) acc True rest
        scan i _ False (':' : rest) = scan (i + 1) (Just i) False rest
        scan i acc False (_ : rest) = scan (i + 1) acc False rest

instance HasCodec TestId where
  codec =
    bimapCodec
      (\t -> maybe (Left ("invalid TestId: " ++ T.unpack t)) Right (parseTestIdFilterArg t))
      renderTestId
      codec

-- | Description text for each step must be non-empty so that 'renderTestId'
-- and 'parseTestIdFilterArg' round-trip: an empty description would render
-- to an unparseable token.
instance Validity TestId where
  validate (TestId steps) = mconcat [declare "step description is non-empty" (not (T.null t)) | (t, _) <- NE.toList steps]

instance GenValid TestId where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
