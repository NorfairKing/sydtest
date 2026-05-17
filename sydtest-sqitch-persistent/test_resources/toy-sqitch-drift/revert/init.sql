-- Revert toy-drift:init from pg
BEGIN;

DROP TABLE IF EXISTS widget;

COMMIT;
