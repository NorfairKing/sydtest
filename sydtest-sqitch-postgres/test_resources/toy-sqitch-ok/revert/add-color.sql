-- Revert toy-ok:add-color from pg
BEGIN;

ALTER TABLE widget DROP COLUMN IF EXISTS color;
ALTER TABLE widget ADD COLUMN IF NOT EXISTS color VARCHAR(7);

COMMIT;
