-- Verify toy-grandfathered:init on pg
BEGIN;

SELECT id, name FROM widget WHERE FALSE;

ROLLBACK;
