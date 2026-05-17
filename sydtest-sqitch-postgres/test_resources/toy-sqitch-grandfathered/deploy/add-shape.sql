-- Deploy toy-grandfathered:add-shape to pg
BEGIN;

ALTER TABLE widget ADD COLUMN IF NOT EXISTS shape TEXT;

COMMIT;
