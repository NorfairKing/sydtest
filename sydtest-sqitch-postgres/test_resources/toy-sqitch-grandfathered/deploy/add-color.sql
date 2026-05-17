-- Deploy toy-grandfathered:add-color to pg
BEGIN;

ALTER TABLE widget ADD COLUMN IF NOT EXISTS color VARCHAR(7);

COMMIT;
