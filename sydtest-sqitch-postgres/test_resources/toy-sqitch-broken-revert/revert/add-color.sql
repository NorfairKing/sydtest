-- Revert toy-broken-revert:add-color from pg
-- BUG: as well as dropping its own added columns, this also drops the
-- `name` column added in the prior `init` migration. The redeploy
-- cannot recover that, so the round-trip schema will differ from the
-- pre-revert snapshot.
BEGIN;

ALTER TABLE widget DROP COLUMN IF EXISTS color;
ALTER TABLE widget DROP COLUMN IF EXISTS shape;
ALTER TABLE widget DROP COLUMN IF EXISTS name;

COMMIT;
