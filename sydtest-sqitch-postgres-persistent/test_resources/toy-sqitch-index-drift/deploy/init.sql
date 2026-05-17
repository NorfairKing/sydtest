-- Deploy toy-index-drift:init to pg
-- Table columns match the persistent Widget model; the extra index on
-- `name` exists only on the sqitch side. Used to assert that the
-- schema-equality check compares indices, not just columns.
BEGIN;

CREATE TABLE IF NOT EXISTS widget (
    id    BIGSERIAL PRIMARY KEY,
    name  TEXT NOT NULL,
    color TEXT
);

CREATE INDEX IF NOT EXISTS widget_name_idx ON widget (name);

COMMIT;
