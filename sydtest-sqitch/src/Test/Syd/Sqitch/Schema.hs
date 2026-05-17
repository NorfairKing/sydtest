{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- The undefined-trick in 'compareSchemaSnapshots' deliberately binds no
-- variables; that's the point — it forces GHC to error when a field is
-- added to SchemaSnapshot.
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}

-- | Helpers for snapshotting a PostgreSQL schema and comparing two
-- snapshots.
module Test.Syd.Sqitch.Schema
  ( -- * Snapshots
    SchemaSnapshot (..),
    querySchema,
    queryColumns,
    queryIndices,
    compareSchemaSnapshots,

    -- * Low-level helpers
    normaliseSchema,
    removeComments,
    align,
    compareSchemas,
  )
where

import Control.Monad (forM_)
import qualified Data.Map.Merge.Strict as Merge
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (Single (..), SqlPersistT, rawSql)
import Test.Syd

-- | Everything that defines the shape of a PostgreSQL @public@ schema
-- for the purposes of equality testing.
--
-- Add new fields here as new categories of schema object grow into
-- scope (constraints, sequences, triggers, views, …). The
-- undefined-trick inside 'compareSchemaSnapshots' will then prompt you
-- to extend the comparison.
data SchemaSnapshot = SchemaSnapshot
  { -- | Columns of every table in @public@, keyed by
    -- @(table_name, column_name)@. See 'queryColumns'.
    schemaSnapshotColumns :: Map (Text, Text) Text,
    -- | Indices of every table in @public@, keyed by
    -- @(table_name, index_name)@. See 'queryIndices'.
    schemaSnapshotIndices :: Map (Text, Text) Text
  }
  deriving (Show, Eq)

-- | Snapshot everything in 'SchemaSnapshot' in one query batch.
querySchema :: (MonadIO m) => SqlPersistT m SchemaSnapshot
querySchema = SchemaSnapshot <$> queryColumns <*> queryIndices

-- | Snapshot the columns of every table in the @public@ schema, keyed
-- by @(table_name, column_name)@. The value is a normalised string
-- containing the schema name, table name, column name, and data type.
queryColumns :: (MonadIO m) => SqlPersistT m (Map (Text, Text) Text)
queryColumns =
  Map.fromList
    . map
      ( \(Single schemaName, Single tableName, Single columnName, Single dataType) ->
          ( (tableName, columnName),
            normaliseSchema $
              schemaName <> "." <> tableName <> "." <> columnName <> " " <> dataType
          )
      )
    <$> rawSql
      "SELECT table_schema, table_name, column_name, data_type FROM information_schema.columns WHERE table_schema = 'public' ORDER BY table_name, ordinal_position"
      []

-- | Snapshot the indices of every table in the @public@ schema, keyed
-- by @(table_name, index_name)@. The value is the normalised
-- @CREATE INDEX@ statement reported by @pg_indexes.indexdef@ — which
-- captures columns, ordering, uniqueness, and any partial-index
-- predicate.
queryIndices :: (MonadIO m) => SqlPersistT m (Map (Text, Text) Text)
queryIndices =
  Map.fromList
    . map
      ( \(Single tableName, Single indexName, Single indexDef) ->
          ((tableName, indexName), normaliseSchema indexDef)
      )
    <$> rawSql
      "SELECT tablename, indexname, indexdef FROM pg_indexes WHERE schemaname = 'public' ORDER BY tablename, indexname"
      []

-- | Compare two 'SchemaSnapshot's field by field, calling
-- 'expectationFailure' on any divergence. The label names the first
-- side so failures read naturally (\"Missing in <label>: ...\" /
-- \"Extra in <label>: ...\").
--
-- The 'undefined' pattern is a deliberate compile-time prompt: when a
-- new field is added to 'SchemaSnapshot', this pattern stops matching
-- and GHC complains, forcing you to extend the comparison.
compareSchemaSnapshots :: String -> SchemaSnapshot -> SchemaSnapshot -> IO ()
compareSchemaSnapshots label actual expected =
  let SchemaSnapshot _ _ = undefined :: SchemaSnapshot
   in do
        context "columns" $
          compareSchemas label $
            align (schemaSnapshotColumns actual) (schemaSnapshotColumns expected)
        context "indices" $
          compareSchemas label $
            align (schemaSnapshotIndices actual) (schemaSnapshotIndices expected)

-- | Strip SQL comments and collapse whitespace so two semantically
-- equal snippets compare equal as 'Text'.
normaliseSchema :: Text -> Text
normaliseSchema =
  Text.strip
    . Text.unwords
    . Text.words
    . Text.replace "\n" " "
    . Text.replace "\r" " "
    . Text.replace "\t" " "
    . removeComments

-- | Drop everything from @--@ to end-of-line, then drop blank lines.
removeComments :: Text -> Text
removeComments =
  Text.unlines
    . filter (not . Text.null)
    . map (fst . Text.breakOn "--")
    . Text.lines

-- | Outer-join two maps, recording for each key which side it came from.
align :: (Ord k) => Map k a -> Map k b -> Map k (Maybe a, Maybe b)
align =
  Merge.merge
    (Merge.mapMissing (\_ a -> (Just a, Nothing)))
    (Merge.mapMissing (\_ b -> (Nothing, Just b)))
    (Merge.zipWithMatched (\_ a b -> (Just a, Just b)))

-- | Walk an aligned schema map and 'expectationFailure' on any
-- divergence. Used internally by 'compareSchemaSnapshots'; exposed for
-- callers that want to compare ad-hoc maps.
compareSchemas ::
  String ->
  Map (Text, Text) (Maybe Text, Maybe Text) ->
  IO ()
compareSchemas label1 aligned =
  forM_ (Map.toList aligned) $ \((typ, name), (v1, v2)) ->
    context (unwords ["In", Text.unpack typ, Text.unpack name]) $
      case (v1, v2) of
        (Just s1, Just s2) -> s1 `shouldBe` s2
        (Nothing, Just sql) ->
          expectationFailure $
            unlines
              [ unwords ["Missing in", label1 <> ":", Text.unpack typ, Text.unpack name],
                Text.unpack sql
              ]
        (Just sql, Nothing) ->
          expectationFailure $
            unlines
              [ unwords ["Extra in", label1 <> ":", Text.unpack typ, Text.unpack name],
                Text.unpack sql
              ]
        (Nothing, Nothing) -> expectationFailure "Impossible: both sides absent"
