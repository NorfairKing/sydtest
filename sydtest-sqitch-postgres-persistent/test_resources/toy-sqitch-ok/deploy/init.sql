-- Deploy toy-ok:init to pg
BEGIN;

CREATE TABLE IF NOT EXISTS widget (
    id   BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

COMMIT;
