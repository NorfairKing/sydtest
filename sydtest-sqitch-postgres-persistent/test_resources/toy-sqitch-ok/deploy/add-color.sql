-- Deploy toy-ok:add-color to pg
BEGIN;

ALTER TABLE widget DROP COLUMN IF EXISTS color;
ALTER TABLE widget ADD COLUMN IF NOT EXISTS color TEXT;

COMMIT;
