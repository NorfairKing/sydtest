-- Revert toy-grandfathered:add-color from pg
BEGIN;

ALTER TABLE widget DROP COLUMN IF EXISTS color;

COMMIT;
