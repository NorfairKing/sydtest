-- Revert toy-broken-revert:init from pg
BEGIN;

DROP TABLE IF EXISTS widget;

COMMIT;
