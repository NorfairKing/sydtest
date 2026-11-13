{-# LANGUAGE OverloadedStrings #-}

-- | Assert that a schema has no index that another index makes unnecessary.
--
-- 'CREATE INDEX IF NOT EXISTS' matches on the name, so nothing in PostgreSQL
-- stops a schema from growing an index that another one already covers: a
-- second index on the same columns under another name, or an index on the
-- leading columns of a unique constraint.
-- Every write to the table then maintains both, and the planner gains nothing.
module Test.Syd.Persistent.Postgresql.RedundantIndices
  ( shouldHaveNoRedundantIndices,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (Single (..), SqlPersistT, rawSql)
import Test.Syd

-- | Fail with the definition of every index that another index on the same
-- table covers, and the definitions of the indices that cover it, outside of
-- PostgreSQL's own schemas.
--
-- An index is covered when the other index has the same access method, the
-- same partial predicate and the same expressions, and its key starts with
-- the whole key of the covered one, column for column, with the same
-- operator class, collation and ordering.
-- Only a btree is searched by a leading part of its key, so for any other
-- access method the keys have to be the same.
-- The covering index also has to contain every column the covered one
-- carries in INCLUDE, or the covered one could answer index-only scans the
-- covering one cannot.
--
-- A unique index enforces something a non-unique one does not, so it is only
-- covered by a unique index on the same key that enforces at least as much:
-- immediately rather than deferred, and with NULLs not distinct if it has
-- them so.
-- An index that backs an exclusion constraint is never reported, because it
-- cannot be dropped without the constraint.
--
-- Two indices that cover each other are both reported, because either one can
-- go.
--
-- A later version may recognise more kinds of redundant index, so a schema
-- that passes this today can fail it after an upgrade.
shouldHaveNoRedundantIndices :: (MonadIO m) => SqlPersistT m ()
shouldHaveNoRedundantIndices = do
  coverings <- redundantIndexCoverings
  case NE.groupWith fst coverings of
    [] -> pure ()
    redundancies ->
      liftIO $
        expectationFailure $
          unlines $
            "Found indices that other indices cover:"
              : concatMap renderRedundancy redundancies

renderRedundancy :: NonEmpty (Text, Text) -> [String]
renderRedundancy coveringsOfOneIndex@((redundant, _) :| _) =
  ""
    : Text.unpack redundant
    : map
      (\(_, covering) -> unwords ["  is covered by", Text.unpack covering])
      (NE.toList coveringsOfOneIndex)

-- | The definition of every redundant index, paired with the definition of
-- each index that covers it, ordered by the first and then the second.
redundantIndexCoverings :: (MonadIO m) => SqlPersistT m [(Text, Text)]
redundantIndexCoverings = do
  rows <-
    rawSql
      "WITH described AS ( \
      \  SELECT x.indexrelid, \
      \         x.indrelid, \
      \         x.indisunique, \
      \         x.indimmediate, \
      \         x.indnullsnotdistinct, \
      \         x.indisvalid, \
      \         c.relam, \
      \         coalesce(pg_get_expr(x.indpred, x.indrelid), '') AS predicate, \
      \         coalesce(pg_get_expr(x.indexprs, x.indrelid), '') AS expressions, \
      \         ( SELECT array_agg(format('%s/%s/%s/%s', k.attnum, k.opclass, k.collation_oid, k.sort_option) ORDER BY k.ord) \
      \           FROM unnest(x.indkey::int2[], x.indclass::oid[], x.indcollation::oid[], x.indoption::int2[]) \
      \                WITH ORDINALITY AS k(attnum, opclass, collation_oid, sort_option, ord) \
      \           WHERE k.ord <= x.indnkeyatts \
      \         ) AS keys, \
      \         ( SELECT coalesce(array_agg(k.attnum), '{}') \
      \           FROM unnest(x.indkey::int2[]) WITH ORDINALITY AS k(attnum, ord) \
      \           WHERE k.ord > x.indnkeyatts \
      \         ) AS included, \
      \         x.indkey::int2[] AS columns \
      \  FROM pg_index x \
      \  JOIN pg_class c ON c.oid = x.indexrelid \
      \  JOIN pg_namespace n ON n.oid = c.relnamespace \
      \  WHERE n.nspname <> 'information_schema' \
      \    AND n.nspname NOT LIKE 'pg\\_%' \
      \) \
      \SELECT pg_get_indexdef(redundant.indexrelid), \
      \       pg_get_indexdef(covering.indexrelid) \
      \FROM described redundant \
      \JOIN described covering \
      \  ON covering.indrelid = redundant.indrelid \
      \ AND covering.indexrelid <> redundant.indexrelid \
      \ AND covering.indisvalid \
      \ AND covering.relam = redundant.relam \
      \ AND covering.predicate = redundant.predicate \
      \ AND covering.expressions = redundant.expressions \
      \ AND cardinality(redundant.keys) <= cardinality(covering.keys) \
      \ AND covering.keys[1:cardinality(redundant.keys)] = redundant.keys \
      \ AND redundant.included <@ covering.columns \
      \WHERE ( cardinality(redundant.keys) = cardinality(covering.keys) \
      \        OR redundant.relam = (SELECT oid FROM pg_am WHERE amname = 'btree') \
      \      ) \
      \  AND ( NOT redundant.indisunique \
      \        OR ( covering.indisunique \
      \             AND cardinality(redundant.keys) = cardinality(covering.keys) \
      \             AND (covering.indimmediate OR NOT redundant.indimmediate) \
      \             AND (covering.indnullsnotdistinct OR NOT redundant.indnullsnotdistinct) \
      \           ) \
      \      ) \
      \  AND NOT EXISTS ( \
      \        SELECT 1 FROM pg_constraint con \
      \        WHERE con.conindid = redundant.indexrelid \
      \          AND con.contype = 'x' \
      \      ) \
      \ORDER BY 1, 2"
      []
  pure $ map (\(Single redundant, Single covering) -> (redundant, covering)) rows
