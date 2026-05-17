-- Verify toy-non-idempotent:init on pg
BEGIN;

SELECT id, name FROM widget WHERE FALSE;

ROLLBACK;
