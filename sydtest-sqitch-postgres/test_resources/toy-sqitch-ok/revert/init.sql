-- Revert toy-ok:init from pg
BEGIN;

DROP TABLE IF EXISTS widget;

COMMIT;
