-- Deploy toy-drift:init to pg
-- Intentionally missing the `color` column that the persistent model has.
BEGIN;

CREATE TABLE IF NOT EXISTS widget (
    id   BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

COMMIT;
