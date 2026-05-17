-- Revert toy-grandfathered:init from pg
BEGIN;

DROP TABLE IF EXISTS widget;

COMMIT;
