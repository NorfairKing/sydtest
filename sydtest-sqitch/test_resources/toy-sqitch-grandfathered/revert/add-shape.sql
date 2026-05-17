-- Revert toy-grandfathered:add-shape from pg
--
-- Intentionally uses the "swap-via-_new" pattern. PostgreSQL keeps the
-- primary-key index's name from the table that owned it before the
-- rename, so after this revert the pkey is named 'widget_new_pkey'
-- rather than 'widget_pkey'. When the per-change round-trip then
-- redeploys add-shape (which only adds a column), the pkey name stays
-- 'widget_new_pkey' — different from the original post(add-shape)
-- snapshot taken on a database where init.sql had created the table
-- directly with pkey 'widget_pkey'.
--
-- This is the kind of legacy quirk the grandfather flag covers: the
-- end-to-end deploy works, but the per-change round-trip would catch
-- the asymmetry. Modern migrations must avoid this; legacy ones don't
-- have to be retrofitted.
BEGIN;

CREATE TABLE widget_new (
    id   SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

INSERT INTO widget_new (id, name) SELECT id, name FROM widget;

DROP TABLE widget;
ALTER TABLE widget_new RENAME TO widget;

COMMIT;
