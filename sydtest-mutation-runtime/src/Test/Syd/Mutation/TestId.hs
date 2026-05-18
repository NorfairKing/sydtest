{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

unescapeDesc :: Text -> Text
unescapeDesc t = T.pack (go (T.unpack t))
  where
    go [] = []
    go ('\\' : c : rest) = c : go rest
    go (c : rest) = c : go rest

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
                [(n, "")] -> Just (unescapeDesc (T.pack rawName), n)
                _ -> Just (unescapeDesc (T.pack s), 0)
        _ -> Just (unescapeDesc (T.pack s), 0)

    splitAtLastUnescapedColon :: String -> Maybe (String, String)
    splitAtLastUnescapedColon s =
      case findUnescapedColons s of
        [] -> Nothing
        is ->
          let i = last is
           in Just (take i s, drop (i + 1) s)

    findUnescapedColons :: String -> [Int]
    findUnescapedColons = go2 0 False
      where
        go2 _ _ [] = []
        go2 i True (_ : rest) = go2 (i + 1) False rest
        go2 i False ('\\' : rest) = go2 (i + 1) True rest
        go2 i False (':' : rest) = i : go2 (i + 1) False rest
        go2 i False (_ : rest) = go2 (i + 1) False rest

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
