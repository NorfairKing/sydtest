-- Deploy toy-non-idempotent:init to pg
BEGIN;

CREATE TABLE widget (
    id   SERIAL PRIMARY KEY,
    name TEXT NOT NULL
);

COMMIT;
