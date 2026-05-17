-- Deploy toy-broken-revert:add-color to pg
BEGIN;

ALTER TABLE widget ADD COLUMN IF NOT EXISTS color VARCHAR(7);
ALTER TABLE widget ADD COLUMN IF NOT EXISTS shape TEXT;

COMMIT;
