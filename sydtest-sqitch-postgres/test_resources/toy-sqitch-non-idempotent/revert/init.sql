-- Revert toy-non-idempotent:init from pg
BEGIN;

DROP TABLE widget;

COMMIT;
