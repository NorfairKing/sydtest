-- Revert toy-ok:add-color@v1 from pg
BEGIN;

ALTER TABLE widget DROP COLUMN IF EXISTS color;

COMMIT;
