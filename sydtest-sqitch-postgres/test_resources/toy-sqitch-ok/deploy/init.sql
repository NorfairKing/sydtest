-- Deploy toy-ok:init to pg
BEGIN;

CREATE TABLE IF NOT EXISTS widget (
    id   SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

COMMIT;
