-- Deploy toy-ok:add-color@v1 to pg
BEGIN;

ALTER TABLE widget ADD COLUMN IF NOT EXISTS color VARCHAR(7);

COMMIT;
